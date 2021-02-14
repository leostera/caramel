(* UTF-8 and HTML entities *)
let characters_htmlentities_descriptions =
(* data extracted from http://www.w3schools.com/
   on December, 18th, 2013 *)
[
" ",
"&#32;",
"space";
"!",
"&#33;",
"exclamation mark";
"\"",
"&#34;",
"quotation mark";
"#",
"&#35;",
"number sign";
"$",
"&#36;",
"dollar sign";
"%",
"&#37;",
"percent sign";
"&amp;",
"&#38;",
"ampersand";
"'",
"&#39;",
"apostrophe";
"(",
"&#40;",
"left parenthesis";
")",
"&#41;",
"right parenthesis";
"*",
"&#42;",
"asterisk";
"+",
"&#43;",
"plus sign";
",",
"&#44;",
"comma";
"-",
"&#45;",
"hyphen";
".",
"&#46;",
"period";
"/",
"&#47;",
"slash";
"0",
"&#48;",
"digit 0";
"1",
"&#49;",
"digit 1";
"2",
"&#50;",
"digit 2";
"3",
"&#51;",
"digit 3";
"4",
"&#52;",
"digit 4";
"5",
"&#53;",
"digit 5";
"6",
"&#54;",
"digit 6";
"7",
"&#55;",
"digit 7";
"8",
"&#56;",
"digit 8";
"9",
"&#57;",
"digit 9";
":",
"&#58;",
"colon";
";",
"&#59;",
"semicolon";
"&lt;",
"&#60;",
"less-than";
"=",
"&#61;",
"equals-to";
"&gt;",
"&#62;",
"greater-than";
"?",
"&#63;",
"question mark";
"@",
"&#64;",
"at sign";
"A",
"&#65;",
"uppercase A";
"B",
"&#66;",
"uppercase B";
"C",
"&#67;",
"uppercase C";
"D",
"&#68;",
"uppercase D";
"E",
"&#69;",
"uppercase E";
"F",
"&#70;",
"uppercase F";
"G",
"&#71;",
"uppercase G";
"H",
"&#72;",
"uppercase H";
"I",
"&#73;",
"uppercase I";
"J",
"&#74;",
"uppercase J";
"K",
"&#75;",
"uppercase K";
"L",
"&#76;",
"uppercase L";
"M",
"&#77;",
"uppercase M";
"N",
"&#78;",
"uppercase N";
"O",
"&#79;",
"uppercase O";
"P",
"&#80;",
"uppercase P";
"Q",
"&#81;",
"uppercase Q";
"R",
"&#82;",
"uppercase R";
"S",
"&#83;",
"uppercase S";
"T",
"&#84;",
"uppercase T";
"U",
"&#85;",
"uppercase U";
"V",
"&#86;",
"uppercase V";
"W",
"&#87;",
"uppercase W";
"X",
"&#88;",
"uppercase X";
"Y",
"&#89;",
"uppercase Y";
"Z",
"&#90;",
"uppercase Z";
"[",
"&#91;",
"left square bracket";
"\\",
"&#92;",
"backslash";
"]",
"&#93;",
"right square bracket";
"^",
"&#94;",
"caret";
"_",
"&#95;",
"underscore";
"`",
"&#96;",
"grave accent";
"a",
"&#97;",
"lowercase a";
"b",
"&#98;",
"lowercase b";
"c",
"&#99;",
"lowercase c";
"d",
"&#100;",
"lowercase d";
"e",
"&#101;",
"lowercase e";
"f",
"&#102;",
"lowercase f";
"g",
"&#103;",
"lowercase g";
"h",
"&#104;",
"lowercase h";
"i",
"&#105;",
"lowercase i";
"j",
"&#106;",
"lowercase j";
"k",
"&#107;",
"lowercase k";
"l",
"&#108;",
"lowercase l";
"m",
"&#109;",
"lowercase m";
"n",
"&#110;",
"lowercase n";
"o",
"&#111;",
"lowercase o";
"p",
"&#112;",
"lowercase p";
"q",
"&#113;",
"lowercase q";
"r",
"&#114;",
"lowercase r";
"s",
"&#115;",
"lowercase s";
"t",
"&#116;",
"lowercase t";
"u",
"&#117;",
"lowercase u";
"v",
"&#118;",
"lowercase v";
"w",
"&#119;",
"lowercase w";
"x",
"&#120;",
"lowercase x";
"y",
"&#121;",
"lowercase y";
"z",
"&#122;",
"lowercase z";
"{",
"&#123;",
"left curly brace";
"|",
"&#124;",
"vertical bar";
"}",
"&#125;",
"right curly brace";
"~",
"&#126;",
"tilde";
"\000",
"&#00;",
"null character";
"\001",
"&#01;",
"start of header";
"\002",
"&#02;",
"start of text";
"\003",
"&#03;",
"end of text";
"\004",
"&#04;",
"end of transmission";
"\005",
"&#05;",
"enquiry";
"\006",
"&#06;",
"acknowledge";
"\007",
"&#07;",
"bell (ring)";
"\008",
"&#08;",
"backspace";
"\009",
"&#09;",
"horizontal tab";
"\010",
"&#10;",
"line feed";
"\011",
"&#11;",
"vertical tab";
"\012",
"&#12;",
"form feed";
"\013",
"&#13;",
"carriage return";
"\014",
"&#14;",
"shift out";
"\015",
"&#15;",
"shift in";
"\016",
"&#16;",
"data link escape";
"\017",
"&#17;",
"device control 1";
"\018",
"&#18;",
"device control 2";
"\019",
"&#19;",
"device control 3";
"\020",
"&#20;",
"device control 4";
"\021",
"&#21;",
"negative acknowledge";
"\022",
"&#22;",
"synchronize";
"\023",
"&#23;",
"end transmission block";
"\024",
"&#24;",
"cancel";
"\025",
"&#25;",
"end of medium";
"\026",
"&#26;",
"substitute";
"\027",
"&#27;",
"escape";
"\028",
"&#28;",
"file separator";
"\029",
"&#29;",
"group separator";
"\030",
"&#30;",
"record separator";
"\031",
"&#31;",
"unit separator";
"\127",
"&#127;",
"delete (rubout)";
"\"",
"&quot;",
"quotation mark";
"'",
"&apos;",
"apostrophe";
"&",
"&amp;",
"ampersand";
"<",
"&lt;",
"less-than";
">",
"&gt;",
"greater-than";
"\xc2\xa0",
"&nbsp;",
"non-breaking space";
"\xc2\xa0",
"&#160;",
"non-breaking space";
"¡",
"&iexcl;",
"inverted exclamation mark";
"¡",
"&#161;",
"inverted exclamation mark";
"¢",
"&cent;",
"cent";
"¢",
"&#162;",
"cent";
"£",
"&pound;",
"pound";
"£",
"&#163;",
"pound";
"¤",
"&curren;",
"currency";
"¤",
"&#164;",
"currency";
"¥",
"&yen;",
"yen";
"¥",
"&#165;",
"yen";
"¦",
"&brvbar;",
"broken vertical bar";
"¦",
"&#166;",
"broken vertical bar";
"§",
"&sect;",
"section";
"§",
"&#167;",
"section";
"¨",
"&uml;",
"spacing diaeresis";
"¨",
"&#168;",
"spacing diaeresis";
"©",
"&copy;",
"copyright";
"©",
"&#169;",
"copyright";
"ª",
"&ordf;",
"feminine ordinal indicator";
"ª",
"&#170;",
"feminine ordinal indicator";
"«",
"&laquo;",
"angle quotation mark (left)";
"«",
"&#171;",
"angle quotation mark (left)";
"¬",
"&not;",
"negation";
"¬",
"&#172;",
"negation";
"�­",
"&shy;",
"soft hyphen";
"�­",
"&#173;",
"soft hyphen";
"®",
"&reg;",
"registered trademark";
"®",
"&#174;",
"registered trademark";
"¯",
"&macr;",
"spacing macron";
"¯",
"&#175;",
"spacing macron";
"°",
"&deg;",
"degree";
"°",
"&#176;",
"degree";
"±",
"&plusmn;",
"plus-or-minus";
"±",
"&#177;",
"plus-or-minus";
"²",
"&sup2;",
"superscript 2";
"²",
"&#178;",
"superscript 2";
"³",
"&sup3;",
"superscript 3";
"³",
"&#179;",
"superscript 3";
"´",
"&acute;",
"spacing acute";
"´",
"&#180;",
"spacing acute";
"µ",
"&micro;",
"micro";
"µ",
"&#181;",
"micro";
"¶",
"&para;",
"paragraph";
"¶",
"&#182;",
"paragraph";
"·",
"&middot;",
"middle dot";
"·",
"&#183;",
"middle dot";
"¸",
"&cedil;",
"spacing cedilla";
"¸",
"&#184;",
"spacing cedilla";
"¹",
"&sup1;",
"superscript 1";
"¹",
"&#185;",
"superscript 1";
"º",
"&ordm;",
"masculine ordinal indicator";
"º",
"&#186;",
"masculine ordinal indicator";
"»",
"&raquo;",
"angle quotation mark (right)";
"»",
"&#187;",
"angle quotation mark (right)";
"¼",
"&frac14;",
"fraction 1/4";
"¼",
"&#188;",
"fraction 1/4";
"½",
"&frac12;",
"fraction 1/2";
"½",
"&#189;",
"fraction 1/2";
"¾",
"&frac34;",
"fraction 3/4";
"¾",
"&#190;",
"fraction 3/4";
"¿",
"&iquest;",
"inverted question mark";
"¿",
"&#191;",
"inverted question mark";
"×",
"&times;",
"multiplication";
"×",
"&#215;",
"multiplication";
"÷",
"&divide;",
"division";
"÷",
"&#247;",
"division";
"À",
"&Agrave;",
"capital a, grave accent";
"À",
"&#192;",
"capital a, grave accent";
"Á",
"&Aacute;",
"capital a, acute accent";
"Á",
"&#193;",
"capital a, acute accent";
"Â",
"&Acirc;",
"capital a, circumflex accent";
"Â",
"&#194;",
"capital a, circumflex accent";
"Ã",
"&Atilde;",
"capital a, tilde";
"Ã",
"&#195;",
"capital a, tilde";
"Ä",
"&Auml;",
"capital a, umlaut mark";
"Ä",
"&#196;",
"capital a, umlaut mark";
"Å",
"&Aring;",
"capital a, ring";
"Å",
"&#197;",
"capital a, ring";
"Æ",
"&AElig;",
"capital ae";
"Æ",
"&#198;",
"capital ae";
"Ç",
"&Ccedil;",
"capital c, cedilla";
"Ç",
"&#199;",
"capital c, cedilla";
"È",
"&Egrave;",
"capital e, grave accent";
"È",
"&#200;",
"capital e, grave accent";
"É",
"&Eacute;",
"capital e, acute accent";
"É",
"&#201;",
"capital e, acute accent";
"Ê",
"&Ecirc;",
"capital e, circumflex accent";
"Ê",
"&#202;",
"capital e, circumflex accent";
"Ë",
"&Euml;",
"capital e, umlaut mark";
"Ë",
"&#203;",
"capital e, umlaut mark";
"Ì",
"&Igrave;",
"capital i, grave accent";
"Ì",
"&#204;",
"capital i, grave accent";
"Í",
"&Iacute;",
"capital i, acute accent";
"Í",
"&#205;",
"capital i, acute accent";
"Î",
"&Icirc;",
"capital i, circumflex accent";
"Î",
"&#206;",
"capital i, circumflex accent";
"Ï",
"&Iuml;",
"capital i, umlaut mark";
"Ï",
"&#207;",
"capital i, umlaut mark";
"Ð",
"&ETH;",
"capital eth, Icelandic";
"Ð",
"&#208;",
"capital eth, Icelandic";
"Ñ",
"&Ntilde;",
"capital n, tilde";
"Ñ",
"&#209;",
"capital n, tilde";
"Ò",
"&Ograve;",
"capital o, grave accent";
"Ò",
"&#210;",
"capital o, grave accent";
"Ó",
"&Oacute;",
"capital o, acute accent";
"Ó",
"&#211;",
"capital o, acute accent";
"Ô",
"&Ocirc;",
"capital o, circumflex accent";
"Ô",
"&#212;",
"capital o, circumflex accent";
"Õ",
"&Otilde;",
"capital o, tilde";
"Õ",
"&#213;",
"capital o, tilde";
"Ö",
"&Ouml;",
"capital o, umlaut mark";
"Ö",
"&#214;",
"capital o, umlaut mark";
"Ø",
"&Oslash;",
"capital o, slash";
"Ø",
"&#216;",
"capital o, slash";
"Ù",
"&Ugrave;",
"capital u, grave accent";
"Ù",
"&#217;",
"capital u, grave accent";
"Ú",
"&Uacute;",
"capital u, acute accent";
"Ú",
"&#218;",
"capital u, acute accent";
"Û",
"&Ucirc;",
"capital u, circumflex accent";
"Û",
"&#219;",
"capital u, circumflex accent";
"Ü",
"&Uuml;",
"capital u, umlaut mark";
"Ü",
"&#220;",
"capital u, umlaut mark";
"Ý",
"&Yacute;",
"capital y, acute accent";
"Ý",
"&#221;",
"capital y, acute accent";
"Þ",
"&THORN;",
"capital THORN, Icelandic";
"Þ",
"&#222;",
"capital THORN, Icelandic";
"ß",
"&szlig;",
"small sharp s, German";
"ß",
"&#223;",
"small sharp s, German";
"à",
"&agrave;",
"small a, grave accent";
"à",
"&#224;",
"small a, grave accent";
"á",
"&aacute;",
"small a, acute accent";
"á",
"&#225;",
"small a, acute accent";
"â",
"&acirc;",
"small a, circumflex accent";
"â",
"&#226;",
"small a, circumflex accent";
"ã",
"&atilde;",
"small a, tilde";
"ã",
"&#227;",
"small a, tilde";
"ä",
"&auml;",
"small a, umlaut mark";
"ä",
"&#228;",
"small a, umlaut mark";
"å",
"&aring;",
"small a, ring";
"å",
"&#229;",
"small a, ring";
"æ",
"&aelig;",
"small ae";
"æ",
"&#230;",
"small ae";
"ç",
"&ccedil;",
"small c, cedilla";
"ç",
"&#231;",
"small c, cedilla";
"è",
"&egrave;",
"small e, grave accent";
"è",
"&#232;",
"small e, grave accent";
"é",
"&eacute;",
"small e, acute accent";
"é",
"&#233;",
"small e, acute accent";
"ê",
"&ecirc;",
"small e, circumflex accent";
"ê",
"&#234;",
"small e, circumflex accent";
"ë",
"&euml;",
"small e, umlaut mark";
"ë",
"&#235;",
"small e, umlaut mark";
"ì",
"&igrave;",
"small i, grave accent";
"ì",
"&#236;",
"small i, grave accent";
"í",
"&iacute;",
"small i, acute accent";
"í",
"&#237;",
"small i, acute accent";
"î",
"&icirc;",
"small i, circumflex accent";
"î",
"&#238;",
"small i, circumflex accent";
"ï",
"&iuml;",
"small i, umlaut mark";
"ï",
"&#239;",
"small i, umlaut mark";
"ð",
"&eth;",
"small eth, Icelandic";
"ð",
"&#240;",
"small eth, Icelandic";
"ñ",
"&ntilde;",
"small n, tilde";
"ñ",
"&#241;",
"small n, tilde";
"ò",
"&ograve;",
"small o, grave accent";
"ò",
"&#242;",
"small o, grave accent";
"ó",
"&oacute;",
"small o, acute accent";
"ó",
"&#243;",
"small o, acute accent";
"ô",
"&ocirc;",
"small o, circumflex accent";
"ô",
"&#244;",
"small o, circumflex accent";
"õ",
"&otilde;",
"small o, tilde";
"õ",
"&#245;",
"small o, tilde";
"ö",
"&ouml;",
"small o, umlaut mark";
"ö",
"&#246;",
"small o, umlaut mark";
"ø",
"&oslash;",
"small o, slash";
"ø",
"&#248;",
"small o, slash";
"ù",
"&ugrave;",
"small u, grave accent";
"ù",
"&#249;",
"small u, grave accent";
"ú",
"&uacute;",
"small u, acute accent";
"ú",
"&#250;",
"small u, acute accent";
"û",
"&ucirc;",
"small u, circumflex accent";
"û",
"&#251;",
"small u, circumflex accent";
"ü",
"&uuml;",
"small u, umlaut mark";
"ü",
"&#252;",
"small u, umlaut mark";
"ý",
"&yacute;",
"small y, acute accent";
"ý",
"&#253;",
"small y, acute accent";
"þ",
"&thorn;",
"small thorn, Icelandic";
"þ",
"&#254;",
"small thorn, Icelandic";
"ÿ",
"&yuml;",
"small y, umlaut mark";
"ÿ",
"&#255;",
"small y, umlaut mark";
"∀",
"&forall;",
"for all";
"∀",
"&#8704;",
"for all";
"∂",
"&part;",
"part";
"∂",
"&#8706;",
"part";
"∃",
"&exist;",
"exists";
"∃",
"&#8707;",
"exists";
"∅",
"&empty;",
"empty";
"∅",
"&#8709;",
"empty";
"∇",
"&nabla;",
"nabla";
"∇",
"&#8711;",
"nabla";
"∈",
"&isin;",
"isin";
"∈",
"&#8712;",
"isin";
"∉",
"&notin;",
"notin";
"∉",
"&#8713;",
"notin";
"∋",
"&ni;",
"ni";
"∋",
"&#8715;",
"ni";
"∏",
"&prod;",
"prod";
"∏",
"&#8719;",
"prod";
"∑",
"&sum;",
"sum";
"∑",
"&#8721;",
"sum";
"−",
"&minus;",
"minus";
"−",
"&#8722;",
"minus";
"∗",
"&lowast;",
"lowast";
"∗",
"&#8727;",
"lowast";
"√",
"&radic;",
"square root";
"√",
"&#8730;",
"square root";
"∝",
"&prop;",
"proportional to";
"∝",
"&#8733;",
"proportional to";
"∞",
"&infin;",
"infinity";
"∞",
"&#8734;",
"infinity";
"∠",
"&ang;",
"angle";
"∠",
"&#8736;",
"angle";
"∧",
"&and;",
"and";
"∧",
"&#8743;",
"and";
"∨",
"&or;",
"or";
"∨",
"&#8744;",
"or";
"∩",
"&cap;",
"cap";
"∩",
"&#8745;",
"cap";
"∪",
"&cup;",
"cup";
"∪",
"&#8746;",
"cup";
"∫",
"&int;",
"integral";
"∫",
"&#8747;",
"integral";
"∴",
"&there4;",
"therefore";
"∴",
"&#8756;",
"therefore";
"∼",
"&sim;",
"similar to";
"∼",
"&#8764;",
"similar to";
"≅",
"&cong;",
"congruent to";
"≅",
"&#8773;",
"congruent to";
"≈",
"&asymp;",
"almost equal";
"≈",
"&#8776;",
"almost equal";
"≠",
"&ne;",
"not equal";
"≠",
"&#8800;",
"not equal";
"≡",
"&equiv;",
"equivalent";
"≡",
"&#8801;",
"equivalent";
"≤",
"&le;",
"less or equal";
"≤",
"&#8804;",
"less or equal";
"≥",
"&ge;",
"greater or equal";
"≥",
"&#8805;",
"greater or equal";
"⊂",
"&sub;",
"subset of";
"⊂",
"&#8834;",
"subset of";
"⊃",
"&sup;",
"superset of";
"⊃",
"&#8835;",
"superset of";
"⊄",
"&nsub;",
"not subset of";
"⊄",
"&#8836;",
"not subset of";
"⊆",
"&sube;",
"subset or equal";
"⊆",
"&#8838;",
"subset or equal";
"⊇",
"&supe;",
"superset or equal";
"⊇",
"&#8839;",
"superset or equal";
"⊕",
"&oplus;",
"circled plus";
"⊕",
"&#8853;",
"circled plus";
"⊗",
"&otimes;",
"circled times";
"⊗",
"&#8855;",
"circled times";
"⊥",
"&perp;",
"perpendicular";
"⊥",
"&#8869;",
"perpendicular";
"⋅",
"&sdot;",
"dot operator";
"⋅",
"&#8901;",
"dot operator";
"Α",
"&Alpha;",
"Alpha";
"Α",
"&#913;",
"Alpha";
"Β",
"&Beta;",
"Beta";
"Β",
"&#914;",
"Beta";
"Γ",
"&Gamma;",
"Gamma";
"Γ",
"&#915;",
"Gamma";
"Δ",
"&Delta;",
"Delta";
"Δ",
"&#916;",
"Delta";
"Ε",
"&Epsilon;",
"Epsilon";
"Ε",
"&#917;",
"Epsilon";
"Ζ",
"&Zeta;",
"Zeta";
"Ζ",
"&#918;",
"Zeta";
"Η",
"&Eta;",
"Eta";
"Η",
"&#919;",
"Eta";
"Θ",
"&Theta;",
"Theta";
"Θ",
"&#920;",
"Theta";
"Ι",
"&Iota;",
"Iota";
"Ι",
"&#921;",
"Iota";
"Κ",
"&Kappa;",
"Kappa";
"Κ",
"&#922;",
"Kappa";
"Λ",
"&Lambda;",
"Lambda";
"Λ",
"&#923;",
"Lambda";
"Μ",
"&Mu;",
"Mu";
"Μ",
"&#924;",
"Mu";
"Ν",
"&Nu;",
"Nu";
"Ν",
"&#925;",
"Nu";
"Ξ",
"&Xi;",
"Xi";
"Ξ",
"&#926;",
"Xi";
"Ο",
"&Omicron;",
"Omicron";
"Ο",
"&#927;",
"Omicron";
"Π",
"&Pi;",
"Pi";
"Π",
"&#928;",
"Pi";
"Ρ",
"&Rho;",
"Rho";
"Ρ",
"&#929;",
"Rho";
"Σ",
"&Sigma;",
"Sigma";
"Σ",
"&#931;",
"Sigma";
"Τ",
"&Tau;",
"Tau";
"Τ",
"&#932;",
"Tau";
"Υ",
"&Upsilon;",
"Upsilon";
"Υ",
"&#933;",
"Upsilon";
"Φ",
"&Phi;",
"Phi";
"Φ",
"&#934;",
"Phi";
"Χ",
"&Chi;",
"Chi";
"Χ",
"&#935;",
"Chi";
"Ψ",
"&Psi;",
"Psi";
"Ψ",
"&#936;",
"Psi";
"Ω",
"&Omega;",
"Omega";
"Ω",
"&#937;",
"Omega";
"α",
"&alpha;",
"alpha";
"α",
"&#945;",
"alpha";
"β",
"&beta;",
"beta";
"β",
"&#946;",
"beta";
"γ",
"&gamma;",
"gamma";
"γ",
"&#947;",
"gamma";
"δ",
"&delta;",
"delta";
"δ",
"&#948;",
"delta";
"ε",
"&epsilon;",
"epsilon";
"ε",
"&#949;",
"epsilon";
"ζ",
"&zeta;",
"zeta";
"ζ",
"&#950;",
"zeta";
"η",
"&eta;",
"eta";
"η",
"&#951;",
"eta";
"θ",
"&theta;",
"theta";
"θ",
"&#952;",
"theta";
"ι",
"&iota;",
"iota";
"ι",
"&#953;",
"iota";
"κ",
"&kappa;",
"kappa";
"κ",
"&#954;",
"kappa";
"λ",
"&lambda;",
"lambda";
"λ",
"&#955;",
"lambda";
"μ",
"&mu;",
"mu";
"μ",
"&#956;",
"mu";
"ν",
"&nu;",
"nu";
"ν",
"&#957;",
"nu";
"ξ",
"&xi;",
"xi";
"ξ",
"&#958;",
"xi";
"ο",
"&omicron;",
"omicron";
"ο",
"&#959;",
"omicron";
"π",
"&pi;",
"pi";
"π",
"&#960;",
"pi";
"ρ",
"&rho;",
"rho";
"ρ",
"&#961;",
"rho";
"ς",
"&sigmaf;",
"sigmaf";
"ς",
"&#962;",
"sigmaf";
"σ",
"&sigma;",
"sigma";
"σ",
"&#963;",
"sigma";
"τ",
"&tau;",
"tau";
"τ",
"&#964;",
"tau";
"υ",
"&upsilon;",
"upsilon";
"υ",
"&#965;",
"upsilon";
"φ",
"&phi;",
"phi";
"φ",
"&#966;",
"phi";
"χ",
"&chi;",
"chi";
"χ",
"&#967;",
"chi";
"ψ",
"&psi;",
"psi";
"ψ",
"&#968;",
"psi";
"ω",
"&omega;",
"omega";
"ω",
"&#969;",
"omega";
"ϑ",
"&thetasym;",
"theta symbol";
"ϑ",
"&#977;",
"theta symbol";
"ϒ",
"&upsih;",
"upsilon symbol";
"ϒ",
"&#978;",
"upsilon symbol";
"ϖ",
"&piv;",
"pi symbol";
"ϖ",
"&#982;",
"pi symbol";
"Œ",
"&OElig;",
"capital ligature OE";
"Œ",
"&#338;",
"capital ligature OE";
"œ",
"&oelig;",
"small ligature oe";
"œ",
"&#339;",
"small ligature oe";
"Š",
"&Scaron;",
"capital S with caron";
"Š",
"&#352;",
"capital S with caron";
"š",
"&scaron;",
"small S with caron";
"š",
"&#353;",
"small S with caron";
"Ÿ",
"&Yuml;",
"capital Y with diaeres";
"Ÿ",
"&#376;",
"capital Y with diaeres";
"ƒ",
"&fnof;",
"f with hook";
"ƒ",
"&#402;",
"f with hook";
"ˆ",
"&circ;",
"modifier letter circumflex accent";
"ˆ",
"&#710;",
"modifier letter circumflex accent";
"˜",
"&tilde;",
"small tilde";
"˜",
"&#732;",
"small tilde";
" ",
"&ensp;",
"en space";
" ",
"&#8194;",
"en space";
" ",
"&emsp;",
"em space";
" ",
"&#8195;",
"em space";
" ",
"&thinsp;",
"thin space";
" ",
"&#8201;",
"thin space";
"‌",
"&zwnj;",
"zero width non-joiner";
"‌",
"&#8204;",
"zero width non-joiner";
"‍",
"&zwj;",
"zero width joiner";
"‍",
"&#8205;",
"zero width joiner";
"‎",
"&lrm;",
"left-to-right mark";
"‎",
"&#8206;",
"left-to-right mark";
"‏",
"&rlm;",
"right-to-left mark";
"‏",
"&#8207;",
"right-to-left mark";
"–",
"&ndash;",
"en dash";
"–",
"&#8211;",
"en dash";
"—",
"&mdash;",
"em dash";
"—",
"&#8212;",
"em dash";
"‘",
"&lsquo;",
"left single quotation mark";
"‘",
"&#8216;",
"left single quotation mark";
"’",
"&rsquo;",
"right single quotation mark";
"’",
"&#8217;",
"right single quotation mark";
"‚",
"&sbquo;",
"single low-9 quotation mark";
"‚",
"&#8218;",
"single low-9 quotation mark";
"“",
"&ldquo;",
"left double quotation mark";
"“",
"&#8220;",
"left double quotation mark";
"”",
"&rdquo;",
"right double quotation mark";
"”",
"&#8221;",
"right double quotation mark";
"„",
"&bdquo;",
"double low-9 quotation mark";
"„",
"&#8222;",
"double low-9 quotation mark";
"†",
"&dagger;",
"dagger";
"†",
"&#8224;",
"dagger";
"‡",
"&Dagger;",
"double dagger";
"‡",
"&#8225;",
"double dagger";
"•",
"&bull;",
"bullet";
"•",
"&#8226;",
"bullet";
"…",
"&hellip;",
"horizontal ellipsis";
"…",
"&#8230;",
"horizontal ellipsis";
"‰",
"&permil;",
"per mille&nbsp;";
"‰",
"&#8240;",
"per mille&nbsp;";
"′",
"&prime;",
"minutes";
"′",
"&#8242;",
"minutes";
"″",
"&Prime;",
"seconds";
"″",
"&#8243;",
"seconds";
"‹",
"&lsaquo;",
"single left angle quotation";
"‹",
"&#8249;",
"single left angle quotation";
"›",
"&rsaquo;",
"single right angle quotation";
"›",
"&#8250;",
"single right angle quotation";
"‾",
"&oline;",
"overline";
"‾",
"&#8254;",
"overline";
"€",
"&euro;",
"euro";
"€",
"&#8364;",
"euro";
"™",
"&trade;",
"trademark";
"™",
"&#8482;",
"trademark";
"™",
"&trade;",
"trademark";
"™",
"&#153;",
"trademark";
"←",
"&larr;",
"left arrow";
"←",
"&#8592;",
"left arrow";
"↑",
"&uarr;",
"up arrow";
"↑",
"&#8593;",
"up arrow";
"→",
"&rarr;",
"right arrow";
"→",
"&#8594;",
"right arrow";
"↓",
"&darr;",
"down arrow";
"↓",
"&#8595;",
"down arrow";
"↔",
"&harr;",
"left right arrow";
"↔",
"&#8596;",
"left right arrow";
"↵",
"&crarr;",
"carriage return arrow";
"↵",
"&#8629;",
"carriage return arrow";
"⌈",
"&lceil;",
"left ceiling";
"⌈",
"&#8968;",
"left ceiling";
"⌉",
"&rceil;",
"right ceiling";
"⌉",
"&#8969;",
"right ceiling";
"⌊",
"&lfloor;",
"left floor";
"⌊",
"&#8970;",
"left floor";
"⌋",
"&rfloor;",
"right floor";
"⌋",
"&#8971;",
"right floor";
"◊",
"&loz;",
"lozenge";
"◊",
"&#9674;",
"lozenge";
"♠",
"&spades;",
"spade";
"♠",
"&#9824;",
"spade";
"♣",
"&clubs;",
"club";
"♣",
"&#9827;",
"club";
"♥",
"&hearts;",
"heart";
"♥",
"&#9829;",
"heart";
"♦",
"&diams;",
"diamond";
"♦",
"&#9830;",
"diamond";
]
