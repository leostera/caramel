use ocaml::*;
use tree_sitter::*;

#[derive(Clone)]
pub struct Language(tree_sitter::Language);

pub struct LanguageError(tree_sitter::LanguageError);

pub struct Range(tree_sitter::Range);

#[derive(Clone)]
pub struct Node(tree_sitter::Node<'static>);

pub struct Parser(tree_sitter::Parser);

#[derive(Clone)]
pub struct Tree(tree_sitter::Tree);

pub struct TreeCursor(tree_sitter::TreeCursor<'static>);

/******************************************************************************/
//
//  High level stuff
//
/******************************************************************************/

#[ocaml::func]
pub fn caml_ts_language_version() -> ocaml::Int {
    LANGUAGE_VERSION as ocaml::Int
}

#[ocaml::func]
pub fn caml_ts_min_compatible_language_version() -> ocaml::Int {
    MIN_COMPATIBLE_LANGUAGE_VERSION as ocaml::Int
}

#[ocaml::func]
pub fn caml_ts_parser_header() -> String {
    PARSER_HEADER.to_string()
}

/******************************************************************************/
//
//  Range
//
/******************************************************************************/

ocaml::custom!(Range {
    finalize: finalize_range,
    compare: compare_range
});

/******************************************************************************/
//
//  Parser
//
/******************************************************************************/

ocaml::custom!(Parser {
    finalize: finalize_parser,
    compare: compare_parser
});

#[ocaml::func]
pub fn caml_ts_parser__new() -> Parser {
    Parser(tree_sitter::Parser::new())
}

#[ocaml::func]
pub fn caml_ts_parser__reset(mut p: ocaml::Pointer<Parser>) {
    p.as_mut().0.reset();
}

#[ocaml::func]
pub fn caml_ts_parser__timeout_micros(mut p: ocaml::Pointer<Parser>) -> ocaml::Int {
    p.as_mut().0.timeout_micros() as ocaml::Int
}

#[ocaml::func]
pub fn caml_ts_parser__set_timeout_micros(mut p: ocaml::Pointer<Parser>, t: u64) {
    p.as_mut().0.set_timeout_micros(t);
}

#[ocaml::func]
pub fn caml_ts_parser__language(mut p: ocaml::Pointer<Parser>) -> Option<Language> {
    p.as_mut().0.language().map(|l| Language(l))
}

#[ocaml::func]
pub fn caml_ts_parser__set_language(
    mut p: ocaml::Pointer<Parser>,
    mut l: ocaml::Pointer<Language>,
) {
    let language = l.as_mut().0;

    let mut parser = &mut p.as_mut().0;

    parser.set_language(language).unwrap();
}

#[ocaml::func]
pub fn caml_ts_parser__parse(
    mut p: ocaml::Pointer<Parser>,
    txt: String,
    _tree: ocaml::Pointer<Option<Tree>>,
) -> Option<Tree> {
    p.as_mut().0.parse(txt, None).map(|t| Tree(t))
}

/******************************************************************************/
//
//  Language
//
/******************************************************************************/

ocaml::custom!(Language {
    finalize: finalize_language,
    compare: compare_language
});

ocaml::custom!(LanguageError {
    finalize: finalize_language_error,
    compare: compare_language_error
});

#[ocaml::func]
pub fn caml_ts_language__version(mut p: ocaml::Pointer<Language>) -> ocaml::Int {
    p.as_mut().0.version() as ocaml::Int
}

#[ocaml::func]
pub fn caml_ts_language__node_kind_count(mut p: ocaml::Pointer<Language>) -> ocaml::Int {
    p.as_mut().0.node_kind_count() as ocaml::Int
}

#[ocaml::func]
pub fn caml_ts_language__node_kind_for_id(
    mut p: ocaml::Pointer<Language>,
    id: u16,
) -> Option<String> {
    p.as_mut().0.node_kind_for_id(id).map(|s| s.to_string())
}

#[ocaml::func]
pub fn caml_ts_language__from_raw_ptr(raw_ptr: Value) -> Language {
    // we'll take the value and cast it into a language
    let raw_lang = raw_ptr.0 as *const tree_sitter::ffi::TSLanguage;
    let lang = tree_sitter::Language(raw_lang);
    Language(lang)
}

/******************************************************************************/
//
//  Node
//
/******************************************************************************/

ocaml::custom!(Node {
    finalize: finalize_node,
    compare: compare_node
});

#[ocaml::func]
pub fn caml_ts_node__to_sexp(mut p: ocaml::Pointer<Node>) -> String {
    p.as_mut().0.to_sexp()
}

#[ocaml::func]
pub fn caml_ts_node__utf8_text(mut p: ocaml::Pointer<Node>, t: String) -> String {
    p.as_mut().0.utf8_text(t.as_bytes()).unwrap().to_string()
}

#[ocaml::func]
pub fn caml_ts_node__walk(mut p: ocaml::Pointer<Node>) -> TreeCursor {
    unsafe { TreeCursor((*p.as_mut_ptr()).0.walk()) }
}

/******************************************************************************/
//
//  Tree
//
/******************************************************************************/

ocaml::custom!(Tree {
    finalize: finalize_tree,
    compare: compare_tree
});

#[ocaml::func]
pub fn caml_ts_tree__root_node(p: ocaml::Pointer<Tree>) -> Node {
    // we don't have enough information about the lifetime of `p` here to be
    // able to do a regular borrow, so we gotta drop to an unsafe pointer
    // and hope for the best
    unsafe { Node((*p.as_ptr()).0.root_node()) }
}

#[ocaml::func]
pub fn caml_ts_tree__walk(mut p: ocaml::Pointer<Tree>) -> TreeCursor {
    unsafe { TreeCursor((*p.as_mut_ptr()).0.walk()) }
}

/******************************************************************************/
//
//  TreeCursor
//
/******************************************************************************/

ocaml::custom!(TreeCursor {
    finalize: finalize_tree_cursor,
    compare: compare_tree_cursor
});

#[ocaml::func]
pub fn caml_ts_tree_cursor__node(mut p: ocaml::Pointer<TreeCursor>) -> Node {
    Node(p.as_ref().0.node())
}

#[ocaml::func]
pub fn caml_ts_tree_cursor__goto_first_child(mut p: ocaml::Pointer<TreeCursor>) -> bool {
    p.as_mut().0.goto_first_child()
}

#[ocaml::func]
pub fn caml_ts_tree_cursor__goto_parent(mut p: ocaml::Pointer<TreeCursor>) -> bool {
    p.as_mut().0.goto_parent()
}

#[ocaml::func]
pub fn caml_ts_tree_cursor__goto_next_sibling(mut p: ocaml::Pointer<TreeCursor>) -> bool {
    p.as_mut().0.goto_next_sibling()
}

////////////////////////////////////////////////////////////////////////////////
///
/// Helpers
///
////////////////////////////////////////////////////////////////////////////////

extern "C" fn compare_parser(_: Value, _: Value) -> i32 {
    0
}

unsafe extern "C" fn finalize_parser(a: Value) {
    let t0 = ocaml::Pointer::<Parser>::from_value(a);
    t0.drop_in_place();
}

extern "C" fn compare_language(_: Value, _: Value) -> i32 {
    0
}

unsafe extern "C" fn finalize_language(a: Value) {
    let t0 = ocaml::Pointer::<Language>::from_value(a);
    t0.drop_in_place();
}

extern "C" fn compare_language_error(_: Value, _: Value) -> i32 {
    0
}

unsafe extern "C" fn finalize_language_error(a: Value) {
    let t0 = ocaml::Pointer::<LanguageError>::from_value(a);
    t0.drop_in_place();
}

extern "C" fn compare_node(_: Value, _: Value) -> i32 {
    0
}

unsafe extern "C" fn finalize_node(a: Value) {
    let t0 = ocaml::Pointer::<Node>::from_value(a);
    t0.drop_in_place();
}

extern "C" fn compare_tree(_: Value, _: Value) -> i32 {
    0
}

unsafe extern "C" fn finalize_tree(a: Value) {
    let t0 = ocaml::Pointer::<Tree>::from_value(a);
    t0.drop_in_place();
}

extern "C" fn compare_tree_cursor(_: Value, _: Value) -> i32 {
    0
}

unsafe extern "C" fn finalize_tree_cursor(a: Value) {
    let t0 = ocaml::Pointer::<TreeCursor>::from_value(a);
    t0.drop_in_place();
}

extern "C" fn compare_range(_: Value, _: Value) -> i32 {
    0
}

unsafe extern "C" fn finalize_range(a: Value) {
    let t0 = ocaml::Pointer::<Range>::from_value(a);
    t0.drop_in_place();
}
