package pathern

import (
	_ "errors"
	_ "fmt"
	"strings"
	"unicode"
	_ "unicode"
	"unicode/utf8"
)

const (
	tkTypLiteral = iota + 1
	tkTypOpenGroup
	tkTypCloseGroup
	tkTypStar
	tkTypStarStar
	tkTypSeparator
	tkTypAlternation
)

type token struct {
	typ       int
	literal   string
	caseSens  bool
	groupName string
	negated   bool
	optional  bool
}

const (
	pTypeRoot = iota
	pTypDirSegment
	pTypFileSegment
	pTypExplicit
	pTypNegated
	pTypAltOuter
	pTypAltInner
	pTypLiteral
	pTypStar
	pTypStarStar
)

type pNode interface {
	Typ() int
	matchSegmentStatic(iseg string, nameValues map[string]string) int
}

type groupNode struct {
	typ      int
	name     string
	optional bool
	nodes    []pNode
}

func (n *groupNode) Typ() int {
	return n.typ
}

func (n *groupNode) Match(path string) (map[string]string, bool) {
	nameValues := make(map[string]string)
	isegs := splitPath(path)
	start := 0
	for {
		if start == len(isegs) {
			if start == len(n.nodes) {
				return nameValues, true
			}
			// check if all remaining nodes can match nothing
			return nil, false
		}
		if start == len(n.nodes) {
			return nil, false
		}
		m := n.nodes[start].matchSegmentStatic(isegs[start], nameValues)
		if m < 0 {
			return nil, false
		}
		if m == 0 {
			break
		}
		start++
	}

	return nil, false
}

func (n *groupNode) matchSegmentStatic(iseg string, nameValues map[string]string) int {
	if len(n.nodes) == 1 {
		m := n.nodes[0].matchSegmentStatic(iseg, nameValues)
		if n.typ == pTypNegated {
			m = -m
		}
		if m > 0 && len(n.name) > 0 {
			nameValues[n.name] = iseg
		}
		return m
	}
	if n.typ == pTypAltOuter {
		for _, an := range n.nodes {
			m := an.matchSegmentStatic(iseg, nameValues)
			if m >= 0 {
				return m
			}
		}
		return -1
	}
	// potNameValues := make(map[string]string)
	// leftLen := 0
	// remSeg := iseg
	// for _, cn := range n.nodes {
	// 	m := cn.matchSegmentStatic(remSeg, 1, potNameValues)
	// 	cnTyp := cn.Typ()
	// 	if (cnTyp == pTypNegated && m > 0) || (cnTyp != pTypNegated && m < 0) {
	// 		return -1
	// 	}
	// 	if m == 0 {
	// 		break
	// 	}
	// 	leftLen += m
	// 	remSeg = iseg[leftLen:]
	// }

	switch n.typ {
	case pTypDirSegment, pTypFileSegment:

	case pTypExplicit:

	case pTypNegated:

	case pTypAltInner:
	}
	return -1
}

type literalNode struct {
	typ           int
	text          string
	optional      bool
	caseSensitive bool
}

func (n *literalNode) Typ() int {
	return n.typ
}

func (n *literalNode) matchSegmentStatic(iseg string, nameValues map[string]string) int {
	dir := 0
	if n.caseSensitive {
		if dir == 0 && iseg == n.text {
			return len(iseg)
		} else if dir > 0 && strings.HasPrefix(iseg, n.text) {
			return len(n.text)
		} else if dir < 0 && strings.HasSuffix(iseg, n.text) {
			return len(n.text)
		}
	} else if dir == 0 {
		if strings.EqualFold(iseg, n.text) {
			return len(iseg)
		}
	} else if dir > 0 {
		if n := hasPrefixFold(iseg, n.text); n > 0 {
			return n
		}
	} else if dir < 0 {
		if n := hasSuffixFold(iseg, n.text); n > 0 {
			return n
		}
	}
	return -1
}

type starNode struct {
	typ int
}

func (n *starNode) Typ() int {
	return n.typ
}

func (n *starNode) matchSegmentStatic(iseg string, nameValues map[string]string) int {
	return len(iseg)
}

type starStarNode struct {
	typ     int
	negated *groupNode
}

func (n *starStarNode) Typ() int {
	return n.typ
}

func (n *starStarNode) matchSegmentStatic(iseg string, nameValues map[string]string) int {
	return 0
}

type textPos struct {
	cur  rune
	next rune
}

func chars(text string, ch chan textPos) {
	pr := rune(0)
	for _, c := range text {
		ch <- textPos{pr, c}
		pr = c
	}
	ch <- textPos{pr, 0}
	close(ch)
}

func lex(text string, tkns chan token) {
	ch := make(chan textPos)
	go chars(text, ch)
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
				tkns <- token{typ: tkTypOpenGroup, groupName: value}
				value = ""
				groupNamePossible = false
				continue
			}
			if p.cur == '"' {
				caseSens = !caseSens
				groupNamePossible = false
				continue
			}
			if p.cur == '<' || p.cur == '>' || p.cur == '*' || p.cur == '?' || p.cur == '|' || p.cur == '/' || p.cur == '\\' {
				tkns <- token{typ: tkTypOpenGroup}
				receive = false
				groupNamePossible = false
				continue
			}
			value += string(p.cur)
			continue
		}
		if p.cur == '"' {
			caseSens = !caseSens
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
		} else {
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
	}
	close(tkns)
}

type parsingState struct {
	list    []pNode
	current pNode
}

func New(text string) *groupNode {
	tkns := make(chan token)
	go lex(text, tkns)

	root := &groupNode{typ: pTypeRoot}
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
	closeSeg := func(typ int) {
		closeAlt()
		prevSeg := &groupNode{typ: typ}
		prevSeg.nodes = append(prevSeg.nodes, ps.list...)
		ps.current = nil
		ps.list = nil
		segParent := stack[len(stack)-2].current.(*groupNode)
		segParent.nodes = append(segParent.nodes, prevSeg)
	}

	for token := range tkns {
		var ns *parsingState = nil
		switch token.typ {
		case tkTypLiteral:
			ps.current = &literalNode{typ: pTypLiteral, text: token.literal, optional: token.optional, caseSensitive: token.caseSens}
		case tkTypOpenGroup:
			if token.negated {
				ps.current = &groupNode{typ: pTypNegated, name: token.groupName}
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
			g.nodes = append(g.nodes, ns.list...)
			ns = nil
		case tkTypStar:
			ps.current = &starNode{typ: pTypStar}
		case tkTypStarStar:
			ss := &starStarNode{typ: pTypStarStar}
			if len(ps.list) == 1 && ps.list[0].Typ() == pTypNegated {
				ss.negated = ps.list[0].(*groupNode)
				ps.list = nil
			} else if len(ps.list) != 0 {
				// error
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
