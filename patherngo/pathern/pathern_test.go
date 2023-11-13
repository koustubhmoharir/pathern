package pathern

import (
	"reflect"
	"testing"
)

func assertMatch(t *testing.T, p *groupNode, pattern string, path string, nameValues map[string]string) {
	vs, ok := p.Match(path)
	if !ok {
		t.Fatalf("Expected path %v to match pattern %v", path, pattern)
	}
	if !reflect.DeepEqual(nameValues, vs) {
		t.Fatalf("Expected %v, got %v when matching path %v with pattern %v", nameValues, vs, path, pattern)
	}
}

func assertNoMatch(t *testing.T, p *groupNode, pattern string, path string) {
	vs, ok := p.Match(path)
	if ok {
		t.Fatalf("Expected path %v to not match pattern %v", path, pattern)
	}
	if len(vs) > 0 {
		t.Fatalf("Expected no name values on failed match of path %v with pattern %v", path, pattern)
	}
}

func assertEqual[V any](t *testing.T, expected V, actual V) {
	if !reflect.DeepEqual(expected, actual) {
		t.Fatalf("Expected %v, got %v", expected, actual)
	}
}

func testLex(t *testing.T, pattern string, expected []token) {
	ch := make(chan token)
	go lex(pattern, ch)
	actual := make([]token, 0, len(expected))
	for tkn := range ch {
		actual = append(actual, tkn)
	}
	assertEqual(t, expected, actual)
}

func TestLexSingleName1(t *testing.T) {
	testLex(t, "abc", []token{{typ: tkTypLiteral, literal: "abc"}})
}
func TestLexOptionalLetter1(t *testing.T) {
	testLex(t, "abc?d", []token{
		{typ: tkTypLiteral, literal: "ab"},
		{typ: tkTypLiteral, literal: "c", optional: true},
		{typ: tkTypLiteral, literal: "d"},
	})
}

func TestLexCaseSensName1(t *testing.T) {
	testLex(t, `"abc"`, []token{{typ: tkTypLiteral, literal: "abc", caseSens: true}})
}
func TestLexCaseSensName2(t *testing.T) {
	testLex(t, `"abc"def`, []token{{typ: tkTypLiteral, literal: "abc", caseSens: true}, {typ: tkTypLiteral, literal: "def"}})
}

func TestLexNamedGroup1(t *testing.T) {
	testLex(t, `<abc:def>`, []token{{typ: tkTypOpenGroup, groupName: "abc"}, {typ: tkTypLiteral, literal: "def"}, {typ: tkTypCloseGroup}})
}

func TestLexNegatedGroup1(t *testing.T) {
	testLex(t, `<!:def>`, []token{{typ: tkTypOpenGroup, negated: true}, {typ: tkTypLiteral, literal: "def"}, {typ: tkTypCloseGroup}})
}

func TestLexAsterisk(t *testing.T) {
	testLex(t, `*`, []token{{typ: tkTypStar}})
}

func TestLexDoubleAsterisk(t *testing.T) {
	testLex(t, `**`, []token{{typ: tkTypStarStar}})
}

func TestLexComplex1(t *testing.T) {
	testLex(t, `<!:td>**/s/**/<debug|release><-x64>?/*.exe`, []token{
		{typ: tkTypOpenGroup, negated: true},
		{typ: tkTypLiteral, literal: "td"},
		{typ: tkTypCloseGroup},
		{typ: tkTypStarStar},
		{typ: tkTypSeparator},
		{typ: tkTypLiteral, literal: "s"},
		{typ: tkTypSeparator},
		{typ: tkTypStarStar},
		{typ: tkTypSeparator},
		{typ: tkTypOpenGroup},
		{typ: tkTypLiteral, literal: "debug"},
		{typ: tkTypAlternation},
		{typ: tkTypLiteral, literal: "release"},
		{typ: tkTypCloseGroup},
		{typ: tkTypOpenGroup},
		{typ: tkTypLiteral, literal: "-x64"},
		{typ: tkTypCloseGroup, optional: true},
		{typ: tkTypSeparator},
		{typ: tkTypStar},
		{typ: tkTypLiteral, literal: ".exe"},
	})
}

func TestParseAlt1(t *testing.T) {
	gn := New(`<test|debug|release><-x64>?`)
	_ = gn
}

func TestSplitPath1(t *testing.T) {
	ss := splitPath(``)
	assertEqual(t, []string{}, ss)
}

func TestSplitPath2(t *testing.T) {
	ss := splitPath(`/`)
	assertEqual(t, []string{}, ss)
}

func TestSplitPath3(t *testing.T) {
	ss := splitPath(`//`)
	assertEqual(t, []string{}, ss)
}

func TestSplitPath4(t *testing.T) {
	ss := splitPath(`/a`)
	assertEqual(t, []string{`a`}, ss)
}

func TestSplitPath5(t *testing.T) {
	ss := splitPath(`/ab`)
	assertEqual(t, []string{`ab`}, ss)
}

func TestSplitPath6(t *testing.T) {
	ss := splitPath(`a/`)
	assertEqual(t, []string{`a`}, ss)
}

func TestSplitPath7(t *testing.T) {
	ss := splitPath(`ab/`)
	assertEqual(t, []string{`ab`}, ss)
}

func TestSplitPath8(t *testing.T) {
	ss := splitPath(`/a/`)
	assertEqual(t, []string{`a`}, ss)
}

func TestSplitPath9(t *testing.T) {
	ss := splitPath(`/ab/`)
	assertEqual(t, []string{`ab`}, ss)
}

func TestSplitPath10(t *testing.T) {
	ss := splitPath(`//a//`)
	assertEqual(t, []string{`a`}, ss)
}

func TestSplitPath11(t *testing.T) {
	ss := splitPath(`//ab//`)
	assertEqual(t, []string{`ab`}, ss)
}

func TestSplitPath12(t *testing.T) {
	ss := splitPath(`/a/b`)
	assertEqual(t, []string{`a`, `b`}, ss)
}

func TestSplitPath13(t *testing.T) {
	ss := splitPath(`/ab/cd`)
	assertEqual(t, []string{`ab`, `cd`}, ss)
}

func TestSplitPath14(t *testing.T) {
	ss := splitPath(`a/b/`)
	assertEqual(t, []string{`a`, `b`}, ss)
}

func TestSplitPath15(t *testing.T) {
	ss := splitPath(`ab/cd/`)
	assertEqual(t, []string{`ab`, `cd`}, ss)
}

func TestSplitPath16(t *testing.T) {
	ss := splitPath(`/a/b/`)
	assertEqual(t, []string{`a`, `b`}, ss)
}

func TestSplitPath17(t *testing.T) {
	ss := splitPath(`/ab/cd/`)
	assertEqual(t, []string{`ab`, `cd`}, ss)
}

func TestSplitPath18(t *testing.T) {
	ss := splitPath(`//a//b//`)
	assertEqual(t, []string{`a`, `b`}, ss)
}

func TestSplitPath19(t *testing.T) {
	ss := splitPath(`//ab//cd//`)
	assertEqual(t, []string{`ab`, `cd`}, ss)
}

func TestMatchSimple1(t *testing.T) {
	ptn := `abc`
	p := New(ptn)
	assertMatch(t, p, ptn, `abc`, map[string]string{})
	assertNoMatch(t, p, ptn, `abc/def`)
	assertNoMatch(t, p, ptn, `abcd`)
}

func TestMatchSimple2(t *testing.T) {
	ptn := `abc/def`
	p := New(ptn)
	assertMatch(t, p, ptn, `abc/def`, map[string]string{})
	assertNoMatch(t, p, ptn, `abc`)
	assertNoMatch(t, p, ptn, `abc/def/ghi`)
}

func TestMatchSimple3(t *testing.T) {
	ptn := `*/def`
	p := New(ptn)
	assertMatch(t, p, ptn, `abc/def`, map[string]string{})
	assertNoMatch(t, p, ptn, `abc`)
	assertNoMatch(t, p, ptn, `abc/def/ghi`)
}

func TestMatchSimple4(t *testing.T) {
	ptn := `*/*`
	p := New(ptn)
	assertMatch(t, p, ptn, `abc/def`, map[string]string{})
	assertNoMatch(t, p, ptn, `abc`)
	assertNoMatch(t, p, ptn, `abc/def/ghi`)
}

func TestMatchAlternation1(t *testing.T) {
	ptn := `abc|def/xyz`
	p := New(ptn)
	assertMatch(t, p, ptn, `abc/xyz`, map[string]string{})
	assertMatch(t, p, ptn, `def/xyz`, map[string]string{})
	assertNoMatch(t, p, ptn, `abc/wxyz`)
	assertNoMatch(t, p, ptn, `def/wxyz`)
	assertNoMatch(t, p, ptn, `abcd/xyz`)
	assertNoMatch(t, p, ptn, `abcdef/xyz`)
}

func TestMatchNegation1(t *testing.T) {
	ptn := `<!:abc>/xyz`
	p := New(ptn)
	assertMatch(t, p, ptn, `abcd/xyz`, map[string]string{})
	assertMatch(t, p, ptn, `def/xyz`, map[string]string{})
	assertNoMatch(t, p, ptn, `abc/xyz`)
	assertNoMatch(t, p, ptn, `abc`)
	assertNoMatch(t, p, ptn, `xyz`)
}
