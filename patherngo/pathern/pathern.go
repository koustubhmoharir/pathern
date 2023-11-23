package pathern

import (
	_ "errors"
	"fmt"
	"strings"
	"unicode"
	_ "unicode"
	"unicode/utf8"
)

type PathPattern interface {
	Match(path string) (map[string]string, bool)
}

func Replace(replacePattern string, values map[string]string) (string, bool) {
	result := ""
	pi := 0
	len := len(replacePattern)
	missing := false
	for i := 0; i < len; i++ {
		c := replacePattern[i]
		if c == '<' {
			if i > pi {
				result += replacePattern[pi:i]
			}
			pi = i + 1
		} else if c == '>' {
			if i > pi {
				name := replacePattern[pi:i]
				val, ok := values[name]
				if ok {
					result += val
				} else {
					missing = true
					result += ("<" + name + ">")
				}
			}
			pi = i + 1
		}
	}
	if pi < len {
		result += replacePattern[pi:]
	}
	return result, !missing
}

type tkTyp int

func (t tkTyp) String() string {
	switch t {
	case tkTypLiteral:
		return "Literal"
	case tkTypOpenGroup:
		return "OpenGroup"
	case tkTypCloseGroup:
		return "CloseGroup"
	case tkTypStar:
		return "Star"
	case tkTypStarStar:
		return "StarStar"
	case tkTypSeparator:
		return "Separator"
	case tkTypAlternation:
		return "Alternation"
	}
	return fmt.Sprintf("tkTyp(%d)", t)
}

const (
	tkTypLiteral tkTyp = iota + 1
	tkTypOpenGroup
	tkTypCloseGroup
	tkTypStar
	tkTypStarStar
	tkTypSeparator
	tkTypAlternation
)

type token struct {
	typ       tkTyp
	literal   string
	caseSens  bool
	groupName string
	negated   bool
	optional  bool
}

type pTyp int

func (p pTyp) String() string {
	switch p {
	case pTypRoot:
		return "Root"
	case pTypDirSegment:
		return "DirSegment"
	case pTypFileSegment:
		return "FileSegment"
	case pTypExplicit:
		return "Explicit"
	case pTypNegatedA:
		return "NegatedAhead"
	case pTypNegatedB:
		return "NegatedBehind"
	case pTypAltOuter:
		return "AltOuter"
	case pTypAltInner:
		return "AltInner"
	case pTypLiteral:
		return "Literal"
	case pTypStar:
		return "Star"
	case pTypStarStar:
		return "StarStar"
	}
	return fmt.Sprintf("pTyp(%d)", p)
}

const (
	pTypRoot pTyp = iota
	pTypDirSegment
	pTypFileSegment
	pTypExplicit
	pTypNegatedA
	pTypNegatedB
	pTypAltOuter
	pTypAltInner
	pTypLiteral
	pTypStar
	pTypStarStar
)

type baseNode struct {
	parent pNode
	index  int
}

func (n *baseNode) Position() (pNode, int) {
	return n.parent, n.index
}

type pNode interface {
	Typ() pTyp
	Link(parent pNode, index int)
	Child(index int) pNode
	Position() (pNode, int)
	extract(valuesByNode map[pNode]string, values map[string]string, key string)
}

type matchSegState struct {
	iseg        string
	remSeg      string
	left        int
	right       int
	nextFwd     pNode
	nextBwd     pNode
	top         pNode
	altStackFwd []int
	altStackBwd []int
	values      map[pNode]string
}

func (ms *matchSegState) nextNode(node pNode, step int, success bool) pNode {
	if !success {
		var altStack []int
		var size int
		if step == 1 {
			altStack = ms.altStackFwd
			size = ms.left
		} else {
			altStack = ms.altStackBwd
			size = ms.right
		}
		altStackLen := len(altStack)
		if altStackLen == 0 || altStack[altStackLen-1] != size {
			return nil
		}
		for {
			parent, index := node.Position()
			if parent.Typ() == pTypAltOuter {
				next := parent.Child(index + abs(step)) //alternation is always left to right
				if next != nil {
					return next
				}
				return nil
			}
			node = parent
		}
	}
	popAltStack := func(p pNode) {
		if p.Typ() == pTypAltOuter {
			if step == 1 {
				ms.altStackFwd = ms.altStackFwd[:len(ms.altStackFwd)-1]
			} else if step == -1 {
				ms.altStackBwd = ms.altStackBwd[:len(ms.altStackBwd)-1]
			}
		}
	}
	for {
		parent, index := node.Position()
		var next pNode
		if parent.Typ() == pTypAltOuter {
			next = parent.Child(index + abs(step))
		} else {
			next = parent.Child(index + step)
		}
		if next != nil {
			return next
		}
		popAltStack(parent)
		if parent == ms.top {
			return nil
		}
		if parent.Typ() == pTypAltInner {
			parent, _ = parent.Position()
			popAltStack(parent)
			if parent == ms.top {
				return nil
			}
		}
		node = parent
	}
}

func (ms *matchSegState) match() bool {
	ms.remSeg = ms.iseg
	topNodes := ms.top.(*groupNode).nodes
	ms.nextFwd = topNodes[0]
	for {
		current := ms.nextFwd
		m := ms.stepForward()
		if m < 0 {
			return false
		}
		if m == 0 {
			break
		}
		if current == ms.nextFwd {
			if ms.left == len(ms.iseg) {
				return true
			}
			if ms.top.Typ() == pTypNegatedA {
				return hasNextInSegment(ms.top, 1)
			}
			// standalone successful negation
			return ms.left == 0
		}
	}
	ms.remSeg = ms.iseg[ms.left:]
	backLen := len(ms.remSeg)
	ms.nextBwd = topNodes[len(topNodes)-1]
	for {
		current := ms.nextBwd
		m := ms.stepBackward()
		if m < 0 {
			return false
		}
		if m == 0 {
			break
		}
		if current == ms.nextBwd {
			if ms.right == backLen {
				return true
			}
			if ms.top.Typ() == pTypNegatedB {
				return hasNextInSegment(ms.top, -1)
			}
			// standalone successful negation
			return ms.right == 0
		}
	}
	if ms.nextFwd == ms.nextBwd {
		if _, ok := ms.nextFwd.(*starNode); ok {
			if ms.values != nil {
				ms.values[ms.nextFwd] = ms.remSeg
			}
			return true
		}
	}
	return false
}

func (ms *matchSegState) extract(values map[string]string) {
	ms.top.extract(ms.values, values, "")
}

func hasNextInSegment(n pNode, step int) bool {
	for {
		if n.Typ() == pTypDirSegment || n.Typ() == pTypFileSegment {
			return false
		}
		parent, index := n.Position()
		if parent.Typ() == pTypAltOuter {
			parent, _ = parent.Position()
		} else {
			next := parent.Child(index + step)
			if next != nil {
				return true
			}
		}
		n = parent
	}
}

func (ms *matchSegState) stepForward() int {
	typ := ms.nextFwd.Typ()
	switch n := ms.nextFwd.(type) {
	case *groupNode:
		if typ == pTypAltInner {
			ms.nextFwd = n.nodes[0]
			return 1
		}
		if typ == pTypExplicit {
			ms.nextFwd = n.nodes[0]
			return 1
		}
		if typ == pTypNegatedA {
			nms := *ms
			nms.iseg = nms.remSeg
			nms.nextFwd = n.nodes[0]
			nms.top = ms.nextFwd
			nms.altStackFwd = nil
			nms.values = nil
			if nms.match() {
				next := ms.nextNode(ms.nextFwd, 1, false)
				if next == nil {
					return -1
				}
				ms.nextFwd = next
				return 1
			} else {
				next := ms.nextNode(ms.nextFwd, 1, true)
				if next != nil {
					ms.nextFwd = next
				}
				return 1
			}
		}
		if typ == pTypNegatedB {
			panic("Not yet implemented")
		}
		if typ == pTypAltOuter {
			ms.altStackFwd = append(ms.altStackFwd, ms.left)
			ms.nextFwd = n.nodes[0]
			return 1
		}
	case *literalNode:
		num := 0
		if n.caseSensitive {
			if strings.HasPrefix(ms.remSeg, n.text) {
				num = len(n.text)
			}
		} else {
			num = hasPrefixFold(ms.remSeg, n.text)
		}
		if num <= 0 && n.optional {
			num = 0
		}
		if num >= 0 {
			ms.left += num
			if ms.values != nil {
				ms.values[n] = ms.remSeg[:num]
			}
			ms.remSeg = ms.remSeg[num:]
			next := ms.nextNode(ms.nextFwd, 1, true)
			if next != nil {
				ms.nextFwd = next
			}
			return 1
		}
		next := ms.nextNode(ms.nextFwd, 1, false)
		if next == nil {
			return -1
		}
		ms.nextFwd = next
		return 1
	case *starNode:
		return 0
	case *starStarNode:
		return 0
	}
	return -1
}

func (ms *matchSegState) stepBackward() int {
	typ := ms.nextBwd.Typ()
	switch n := ms.nextBwd.(type) {
	case *groupNode:
		if typ == pTypAltInner {
			ms.nextBwd = n.nodes[len(n.nodes)-1]
			return 1
		}
		if typ == pTypExplicit {
			ms.nextBwd = n.nodes[len(n.nodes)-1]
			return 1
		}
		if typ == pTypNegatedA {
			panic("Not yet implemented")
		}
		if typ == pTypNegatedB {
			nms := *ms
			nms.iseg = nms.remSeg
			nms.nextBwd = n.nodes[len(n.nodes)-1]
			nms.top = ms.nextBwd
			nms.altStackFwd = nil
			if nms.match() {
				next := ms.nextNode(ms.nextBwd, -1, false)
				if next == nil {
					return -1
				}
				ms.nextBwd = next
				return 1
			} else {
				next := ms.nextNode(ms.nextBwd, -1, true)
				if next != nil {
					ms.nextBwd = next
				}
				return 1
			}
		}
		if typ == pTypAltOuter {
			ms.altStackFwd = append(ms.altStackFwd, ms.right)
			ms.nextBwd = n.nodes[0]
			return 1
		}
	case *literalNode:
		num := 0
		if n.caseSensitive {
			if strings.HasSuffix(ms.remSeg, n.text) {
				num = len(n.text)
			}
		} else {
			num = hasSuffixFold(ms.remSeg, n.text)
		}
		if num > 0 {
			ms.right += num
			if ms.values != nil {
				ms.values[n] = ms.remSeg[len(ms.remSeg)-num:]
			}
			ms.remSeg = ms.remSeg[:len(ms.remSeg)-num]
			next := ms.nextNode(ms.nextBwd, -1, true)
			if next != nil {
				ms.nextBwd = next
			}
			return 1
		}
		next := ms.nextNode(ms.nextBwd, -1, false)
		if next == nil {
			return -1
		}
		ms.nextBwd = next
		return 1
	case *starNode:
		return 0
	case *starStarNode:
		return 0
	}
	return -1
}

type groupNode struct {
	baseNode
	typ      pTyp
	name     string
	optional bool
	nodes    []pNode
}

func (n *groupNode) Typ() pTyp {
	return n.typ
}

func (n *groupNode) Link(parent pNode, index int) {
	n.parent = parent
	n.index = index
	for i, cn := range n.nodes {
		cn.Link(n, i)
	}
}

func (n *groupNode) Child(index int) pNode {
	if index >= 0 && index < len(n.nodes) {
		return n.nodes[index]
	}
	return nil
}

func (n *groupNode) extract(valuesByNode map[pNode]string, values map[string]string, key string) {
	if n.name == "" {
		for _, cn := range n.nodes {
			cn.extract(valuesByNode, values, key)
		}
	} else {
		for _, cn := range n.nodes {
			cn.extract(valuesByNode, values, n.name)
		}
		if key != "" {
			v, _ := values[n.name]
			values[key] += v
		}
	}
}

type matchPathState struct {
	remSegs  []string
	remNodes []pNode
	values   map[string]string
}

func (mp *matchPathState) matchOneOnOne(dir int) int {
	for {
		if len(mp.remSegs) == 0 {
			if len(mp.remNodes) == 0 {
				return 1
			}
		} else if len(mp.remNodes) == 0 {
			return -1
		}
		ni := If(dir == 1, 0, len(mp.remNodes)-1)
		node := mp.remNodes[ni].(*groupNode)
		if node.nodes[0].Typ() == pTypStarStar {
			return 0
		}
		if len(mp.remSegs) == 0 {
			return -1
		}
		si := If(dir == 1, 0, len(mp.remSegs)-1)
		ms := matchSegState{iseg: mp.remSegs[si], top: node, values: make(map[pNode]string)}
		m := ms.match()
		if !m {
			return -1
		}
		ms.extract(mp.values)
		if dir == 1 {
			mp.remNodes = mp.remNodes[1:]
			mp.remSegs = mp.remSegs[1:]
		} else {
			mp.remNodes = mp.remNodes[:ni]
			mp.remSegs = mp.remSegs[:si]
		}
	}
}

func (n *groupNode) Match(path string) (map[string]string, bool) {
	nameValues := make(map[string]string)
	mp := matchPathState{remSegs: splitPath(path), remNodes: n.nodes, values: nameValues}
	f := mp.matchOneOnOne(1)
	if f < 0 {
		return nil, false
	}
	if f > 0 {
		return nameValues, true
	}
	b := mp.matchOneOnOne(-1)
	if b < 0 {
		return nil, false
	}
	if b > 0 {
		return nameValues, true
	}
	if len(mp.remNodes) == 1 {
		return nameValues, true
	}
	return nil, false
}

type literalNode struct {
	baseNode
	typ           pTyp
	text          string
	optional      bool
	caseSensitive bool
}

func (n *literalNode) Typ() pTyp {
	return n.typ
}

func (n *literalNode) Link(parent pNode, index int) {
	n.parent = parent
	n.index = index
}

func (n *literalNode) Child(index int) pNode {
	return nil
}

func (n *literalNode) extract(valuesByNode map[pNode]string, values map[string]string, key string) {
	if key != "" {
		v, _ := valuesByNode[n]
		values[key] += v
	}
}

type starNode struct {
	baseNode
	typ pTyp
}

func (n *starNode) Typ() pTyp {
	return n.typ
}

func (n *starNode) Link(parent pNode, index int) {
	n.parent = parent
	n.index = index
}

func (n *starNode) Child(index int) pNode {
	return nil
}

func (n *starNode) extract(valuesByNode map[pNode]string, values map[string]string, key string) {
	if key != "" {
		v, _ := valuesByNode[n]
		values[key] += v
	}
}

type starStarNode struct {
	baseNode
	typ     pTyp
	negated *groupNode
}

func (n *starStarNode) Typ() pTyp {
	return n.typ
}

func (n *starStarNode) Link(parent pNode, index int) {
	n.parent = parent
	n.index = index
	if n.negated != nil {
		n.negated.Link(n, 0)
	}
}

func (n *starStarNode) Child(index int) pNode {
	if index == 0 && n.negated != nil {
		return n.negated
	}
	return nil
}

func (n *starStarNode) extract(valuesByNode map[pNode]string, values map[string]string, key string) {
}

type textPos struct {
	cur      rune
	next     rune
	nextnext rune
}

func chars(text string, ch chan textPos) {
	pr1 := rune(0)
	pr2 := rune(0)
	for _, c := range text {
		ch <- textPos{pr1, pr2, c}
		pr1 = pr2
		pr2 = c
	}
	ch <- textPos{pr1, pr2, 0}
	ch <- textPos{pr2, 0, 0}
	close(ch)
}

func lex(text string, tkns chan token) {
	ch := make(chan textPos)
	go chars(text, ch)
	<-ch
	<-ch
	skippingInitSlash := true
	caseSens := false
	valueCaseSens := false
	value := ""
	groupNamePossible := false
	ok := true
	receive := true
	var p textPos
	sendLiteral := func() {
		if len(value) > 0 {
			tkns <- token{typ: tkTypLiteral, literal: value, caseSens: valueCaseSens}
		}
		value = ""
	}
	for {
		if receive {
			p, ok = <-ch
		} else {
			receive = true
		}
		if !ok {
			break
		}
		if skippingInitSlash {
			if p.cur == '/' || p.cur == '\\' {
				continue
			} else {
				skippingInitSlash = false
			}
		}
		if groupNamePossible {
			if p.cur == ':' {
				if p.next == '!' && p.nextnext == '>' {
					tkns <- token{typ: tkTypOpenGroup}
					receive = false
				} else {
					tkns <- token{typ: tkTypOpenGroup, groupName: value}
					value = ""
				}
				groupNamePossible = false
				continue
			}
			if p.cur == '"' {
				tkns <- token{typ: tkTypOpenGroup}
				caseSens = !caseSens
				groupNamePossible = false
				// We don't end any previous literal here. If case changes in the middle of a literal, we will end the literal when a character with the new case is encountered.
				continue
			}
			if p.cur == '<' || p.cur == '>' || p.cur == '*' || p.cur == '?' || p.cur == '|' || p.cur == '/' || p.cur == '\\' {
				tkns <- token{typ: tkTypOpenGroup}
				receive = false
				groupNamePossible = false
				continue
			}
			value += string(p.cur)
			valueCaseSens = caseSens
			continue
		}
		if p.cur == '"' {
			caseSens = !caseSens
			// Unless we are at the end, we don't end any previous literal here. If case changes in the middle of a literal, we will end the literal when a character with the new case is encountered.
			if p.next == 0 {
				sendLiteral()
			}
			continue
		}
		if p.cur == '<' {
			sendLiteral()
			if p.next == '!' {
				p, ok = <-ch
				if p.next == ':' {
					tkns <- token{typ: tkTypOpenGroup, negated: true}
					p, ok = <-ch
				} else {
					value = "!"
					valueCaseSens = caseSens
				}
				continue
			}
			groupNamePossible = true
			continue
		}
		if p.cur == ':' {
			sendLiteral()
			if p.next == '!' {
				p, ok = <-ch
				if p.next == '>' {
					p, ok = <-ch
					if p.next == '?' {
						p, ok = <-ch
						tkns <- token{typ: tkTypCloseGroup, optional: true, negated: true}
					} else {
						tkns <- token{typ: tkTypCloseGroup, negated: true}
					}
				}
				panic("expected >")
			}
			panic("!>")
		}
		if p.cur == '>' {
			sendLiteral()
			if p.next == '?' {
				p, ok = <-ch
				tkns <- token{typ: tkTypCloseGroup, optional: true}
			} else {
				tkns <- token{typ: tkTypCloseGroup}
			}
			continue
		}
		if p.cur == '*' {
			sendLiteral()
			if p.next == '*' {
				p, ok = <-ch
				tkns <- token{typ: tkTypStarStar}
			} else {
				tkns <- token{typ: tkTypStar}
			}
			continue
		}
		if p.cur == '|' {
			sendLiteral()
			tkns <- token{typ: tkTypAlternation}
			continue
		}
		if p.cur == '/' || p.cur == '\\' {
			sendLiteral()
			tkns <- token{typ: tkTypSeparator}
			continue
		}
		if p.next == '?' {
			if len(value) > 0 {
				tkns <- token{typ: tkTypLiteral, literal: value, caseSens: valueCaseSens}
			}
			tkns <- token{typ: tkTypLiteral, literal: string(p.cur), caseSens: caseSens, optional: true}
			p, ok = <-ch
			value = ""
			continue
		}
		if len(value) > 0 && valueCaseSens != caseSens {
			tkns <- token{typ: tkTypLiteral, literal: value, caseSens: valueCaseSens}
			value = ""
		}
		value += string(p.cur)
		valueCaseSens = caseSens
		if p.next == 0 {
			sendLiteral()
		}
	}
	close(tkns)
}

type parsingState struct {
	list    []pNode
	current pNode
}

func New(text string) PathPattern {
	tkns := make(chan token)
	go lex(text, tkns)

	root := &groupNode{typ: pTypRoot}
	stack := make([]*parsingState, 0, 2)
	ps := &parsingState{}
	ps.current = root
	stack = append(stack, ps)
	ps = &parsingState{}
	stack = append(stack, ps)

	closeAlt := func() {
		if len(stack) > 1 && stack[len(stack)-2].current.Typ() == pTypAltOuter {
			prevInner := &groupNode{typ: pTypAltInner}
			prevInner.nodes = append(prevInner.nodes, ps.list...)
			ps.list = nil
			outer := stack[len(stack)-2].current.(*groupNode)
			outer.nodes = append(outer.nodes, prevInner)
			stack = stack[0 : len(stack)-1]
			ps = stack[len(stack)-1]
		}
	}
	closeSeg := func(typ pTyp) {
		closeAlt()
		if len(ps.list) > 0 {
			prevSeg := &groupNode{typ: typ}
			prevSeg.nodes = append(prevSeg.nodes, ps.list...)
			segParent := stack[len(stack)-2].current.(*groupNode)
			segParent.nodes = append(segParent.nodes, prevSeg)
		}
		ps.current = nil
		ps.list = nil
	}

	for token := range tkns {
		var ns *parsingState = nil
		switch token.typ {
		case tkTypLiteral:
			ps.current = &literalNode{typ: pTypLiteral, text: token.literal, optional: token.optional, caseSensitive: token.caseSens}
		case tkTypOpenGroup:
			if token.negated {
				ps.current = &groupNode{typ: pTypNegatedA}
			} else {
				ps.current = &groupNode{typ: pTypExplicit, name: token.groupName}
			}
			ns = &parsingState{}
		case tkTypCloseGroup:
			closeAlt()
			ns = ps
			stack = stack[0 : len(stack)-1]
			ps = stack[len(stack)-1]
			g := ps.current.(*groupNode)
			g.optional = token.optional
			if token.negated {
				if g.typ == pTypNegatedA {
					panic("negated group can be look-ahead or look-behind but not both")
				}
				g.typ = pTypNegatedB
			}
			g.nodes = append(g.nodes, ns.list...)
			ns = nil
		case tkTypStar:
			ps.current = &starNode{typ: pTypStar}
		case tkTypStarStar:
			ss := &starStarNode{typ: pTypStarStar}
			if len(ps.list) == 1 && (ps.list[0].Typ() == pTypNegatedA || ps.list[0].Typ() == pTypNegatedB) {
				ss.negated = ps.list[0].(*groupNode)
				ps.list = nil
			} else if len(ps.list) != 0 {
				panic("only a single negated group can be before a **")
			}
			ps.current = ss
		case tkTypAlternation:
			prevInner := &groupNode{typ: pTypAltInner}
			prevInner.nodes = append(prevInner.nodes, ps.list...)
			ps.list = nil
			if len(stack) > 1 && stack[len(stack)-2].current.Typ() == pTypAltOuter {
				outer := stack[len(stack)-2].current.(*groupNode)
				outer.nodes = append(outer.nodes, prevInner)
			} else {
				outer := &groupNode{typ: pTypAltOuter}
				outer.nodes = append(outer.nodes, prevInner)
				ps.current = outer
				ps.list = append(ps.list, ps.current)
				ns = &parsingState{}
			}
		case tkTypSeparator:
			closeSeg(pTypDirSegment)
		}
		if ns != nil {
			stack = append(stack, ns)
			ps = ns
		} else if ps.current != nil {
			ps.list = append(ps.list, ps.current)
		}
	}
	if len(stack) == 2 {
		closeSeg(pTypFileSegment)
	}
	root.Link(nil, -1)
	return root
}

func splitPath(path string) []string {
	segments := make([]string, 0, 4)
	for {
		end := strings.IndexAny(path, "/\\")
		if end < 0 {
			if len(path) > 0 {
				segments = append(segments, path)
			}
			break
		}
		if end > 0 {
			segments = append(segments, path[:end])
		}
		path = path[end+1:]
	}
	return segments
}

// hasPrefixFold checks whether s has a prefix of t.
// it returns the number of bytes in s that make up the prefix, -1 if t is not a prefix
// s and t are interpreted as UTF-8 strings and simple Unicode case-folding is used.
func hasPrefixFold(s, t string) int {
	// ASCII fast path
	i := 0
	for ; i < len(s) && i < len(t); i++ {
		sr := s[i]
		tr := t[i]
		if sr|tr >= utf8.RuneSelf {
			goto hasUnicode
		}

		// Easy case.
		if tr == sr {
			continue
		}

		// Make sr < tr to simplify what follows.
		if tr < sr {
			tr, sr = sr, tr
		}
		// ASCII only, sr/tr must be upper/lower case
		if 'A' <= sr && sr <= 'Z' && tr == sr+'a'-'A' {
			continue
		}
		return -1
	}
	// Check if we've exhausted t.
	if len(s) >= len(t) {
		return len(t)
	}
	return -1

hasUnicode:
	s = s[i:]
	t = t[i:]
	num := i
	for {
		// If t is exhausted, it is a prefix
		if len(t) == 0 {
			return num
		}
		// If t is exhausted, t is not a prefix
		if len(s) == 0 {
			return -1
		}

		// Extract first rune from first string.
		var sr rune
		if s[0] < utf8.RuneSelf {
			sr, s = rune(s[0]), s[1:]
			num++
		} else {
			r, size := utf8.DecodeRuneInString(s)
			sr, s = r, s[size:]
			num += size
		}

		// Extract first rune from second string.
		var tr rune
		if t[0] < utf8.RuneSelf {
			tr, t = rune(t[0]), t[1:]
		} else {
			r, size := utf8.DecodeRuneInString(t)
			tr, t = r, t[size:]
		}

		// If they match, keep going; if not, return false.

		// Easy case.
		if tr == sr {
			continue
		}

		// Make sr < tr to simplify what follows.
		if tr < sr {
			tr, sr = sr, tr
		}
		// Fast check for ASCII.
		if tr < utf8.RuneSelf {
			// ASCII only, sr/tr must be upper/lower case
			if 'A' <= sr && sr <= 'Z' && tr == sr+'a'-'A' {
				continue
			}
			return -1
		}

		// General case. SimpleFold(x) returns the next equivalent rune > x
		// or wraps around to smaller values.
		r := unicode.SimpleFold(sr)
		for r != sr && r < tr {
			r = unicode.SimpleFold(r)
		}
		if r == tr {
			continue
		}
		return -1
	}
}

// hasSuffixFold checks whether s has a suffix of t.
// it returns the number of bytes in s that make up the suffix, -1 if t is not a suffix
// s and t are interpreted as UTF-8 strings and simple Unicode case-folding is used.
func hasSuffixFold(s, t string) int {
	// ASCII fast path
	i := 0
	ls := len(s) - 1
	ts := len(t) - 1
	for ; i < len(s) && i < len(t); i++ {
		sr := s[ls-i]
		tr := t[ts-i]
		if sr|tr >= utf8.RuneSelf {
			goto hasUnicode
		}

		// Easy case.
		if tr == sr {
			continue
		}

		// Make sr < tr to simplify what follows.
		if tr < sr {
			tr, sr = sr, tr
		}
		// ASCII only, sr/tr must be upper/lower case
		if 'A' <= sr && sr <= 'Z' && tr == sr+'a'-'A' {
			continue
		}
		return -1
	}
	// Check if we've exhausted t.
	if len(s) >= len(t) {
		return len(t)
	}
	return -1

hasUnicode:
	s = s[:ls-i+1]
	t = t[:ts-i+1]
	si := len(s) - 1
	ti := len(t) - 1
	num := i
	for {
		// If t is exhausted, it is a prefix
		if ti < 0 {
			return num
		}
		// If s is exhausted, it is not a prefix
		if si < 0 {
			return -1
		}

		// Extract last rune from first string.
		var sr rune
		if s[si] < utf8.RuneSelf {
			sr, s, si = rune(s[si]), s[:si], si-1
			num++
		} else {
			r, size := utf8.DecodeLastRuneInString(s)
			sr, s, si = r, s[:si-size+1], si-size
			num += size
		}

		// Extract first rune from second string.
		var tr rune
		if t[ti] < utf8.RuneSelf {
			tr, t, ti = rune(t[ti]), t[:ti], ti-1
		} else {
			r, size := utf8.DecodeLastRuneInString(t)
			tr, t, ti = r, t[:ti-size+1], ti-size
		}

		// If they match, keep going; if not, return false.

		// Easy case.
		if tr == sr {
			continue
		}

		// Make sr < tr to simplify what follows.
		if tr < sr {
			tr, sr = sr, tr
		}
		// Fast check for ASCII.
		if tr < utf8.RuneSelf {
			// ASCII only, sr/tr must be upper/lower case
			if 'A' <= sr && sr <= 'Z' && tr == sr+'a'-'A' {
				continue
			}
			return -1
		}

		// General case. SimpleFold(x) returns the next equivalent rune > x
		// or wraps around to smaller values.
		r := unicode.SimpleFold(sr)
		for r != sr && r < tr {
			r = unicode.SimpleFold(r)
		}
		if r == tr {
			continue
		}
		return -1
	}
}

func abs(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func If[T any](condition bool, whenTrue T, whenFalse T) T {
	if condition {
		return whenTrue
	}
	return whenFalse
}
