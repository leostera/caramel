(**
--- Day 2: Password Philosophy ---

Your flight departs in a few days from the coastal airport; the easiest way
down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
"Something's wrong with our computers; we can't log in!" You ask if you can
take a look.

Their password database seems to be a little corrupted: some of the passwords
wouldn't have been allowed by the Official Toboggan Corporate Policy that was
in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of
passwords (according to the corrupted database) and the corporate policy when
that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy
indicates the lowest and highest number of times a given letter must appear for
the password to be valid. For example, 1-3 a means that the password must
contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is
not; it contains no instances of b, but needs at least 1. The first and third
passwords are valid: they contain one a or nine c, both within the limits of
their respective policies.

How many passwords are valid according to their policies?
*)

type password_req = {
	letter: string;
	min: int;
	max: int;
}

type validation_result = | Valid of int | Invalid of int

let count_min_max { letter; min; max }  pwd =
  let parts = Binary.bin_to_list pwd in
	let result = Lists.foldl (fun c acc ->
    if letter = (Binary.list_to_bin [c])
		then acc + 1
		else acc
	) 0 parts in
	if result >= min && result <= max
	then Valid result
	else Invalid result


type position_counter =
  | Found_first
  | Found_last
  | Not_found
  | Too_many

let is_in_position { letter; min; max }  pwd =
  let parts = Binary.bin_to_list pwd in
	let (result, idx) = Lists.foldl (fun c (counter, idx) ->
    let same_char = letter = (Binary.list_to_bin [c]) in

    match counter with
    | Not_found ->
        if idx = min && same_char
        then (Found_first, idx+1)
        else if idx = max && same_char
        then (Found_last, idx+1)
        else (Not_found, idx+1)

    | Found_first ->
        if idx = max && same_char
        then (Too_many, 0)
        else (Found_first, idx+1)

    | Found_last ->
        if idx = min && same_char
        then (Too_many, 0)
        else (Found_last, idx+1)

    | Too_many -> (Too_many, 0)

	) (Not_found, 1) parts in
	match result with
  | Found_first -> Valid idx
  | Found_last -> Valid idx
  | _ -> Invalid 0

let count_valid req pwds run_validation =
	let results = Lists.map (fun pwd -> run_validation req pwd) pwds in
	Lists.foldl (fun r acc ->
		match r with
		| Valid _ -> acc + 1
    | _ -> acc
	) 0 results


let count_all validation data =
  Lists.foldl (fun (r, pwds) acc ->
    (count_valid r pwds validation) + acc
  ) 0 data

let run () =
  let data = [
({ min = 3; max = 6; letter = "s" }, [ "ssdsssss" ]);
({ min = 17; max = 19; letter = "f" }, [ "cnffsfffzhfnsffttms" ]);
({ min = 8; max = 11; letter = "c" }, [ "tzvtwncnwvwttp" ]);
({ min = 8; max = 10; letter = "r" }, [ "rwrrtrvttrrrr" ]);
({ min = 1; max = 2; letter = "p" }, [ "zhpjph" ]);
({ min = 4; max = 6; letter = "l" }, [ "pldnxv" ]);
({ min = 2; max = 12; letter = "k" }, [ "kkktrkwrbtck" ]);
({ min = 14; max = 15; letter = "t" }, [ "tttttttttttnztttg" ]);
({ min = 1; max = 3; letter = "g" }, [ "dgxg" ]);
({ min = 4; max = 7; letter = "c" }, [ "tmxbccmcvgpp" ]);
({ min = 2; max = 16; letter = "r" }, [ "rrrrrrrrrrrrrrrrr" ]);
({ min = 10; max = 11; letter = "b" }, [ "bbblxbbrbbbblgwcxj" ]);
({ min = 1; max = 7; letter = "s" }, [ "ssshzwpss" ]);
({ min = 7; max = 13; letter = "k" }, [ "kkzkvgkdqskktkkkkk" ]);
({ min = 1; max = 3; letter = "l" }, [ "wjldclpl" ]);
({ min = 15; max = 17; letter = "n" }, [ "tqcsxnwhfnlmpcnjnlsn" ]);
({ min = 8; max = 9; letter = "m" }, [ "mscmmxmkm" ]);
({ min = 3; max = 4; letter = "n" }, [ "fngn" ]);
({ min = 14; max = 15; letter = "g" }, [ "ggrbggglmktgvjg" ]);
({ min = 5; max = 6; letter = "p" }, [ "ppppvxpp" ]);
({ min = 4; max = 7; letter = "b" }, [ "gbbbrbqbwbfjqjg" ]);
({ min = 8; max = 12; letter = "s" }, [ "vssssnsssjsssjsjs" ]);
({ min = 6; max = 11; letter = "b" }, [ "bbbbbbtbbjdbbbb" ]);
({ min = 16; max = 17; letter = "d" }, [ "dddqdmdqdddsdqbmd" ]);
({ min = 18; max = 20; letter = "d" }, [ "bsddtgwddddddzdzdbdd" ]);
({ min = 10; max = 11; letter = "c" }, [ "cccpcccccrkc" ]);
({ min = 3; max = 8; letter = "d" }, [ "hddccvldwdthrc" ]);
({ min = 7; max = 13; letter = "f" }, [ "ftmwxpcsfxzqv" ]);
({ min = 12; max = 17; letter = "w" }, [ "wwfwwwtwwwwwwwwwww" ]);
({ min = 9; max = 10; letter = "b" }, [ "bkbbbnbrsblvbrbpgb" ]);
({ min = 15; max = 16; letter = "g" }, [ "ggggggggggggggggg" ]);
({ min = 2; max = 9; letter = "v" }, [ "rnnvvwrvvvvvjvlr" ]);
({ min = 4; max = 5; letter = "k" }, [ "qkkjwmkk" ]);
({ min = 3; max = 8; letter = "v" }, [ "vmkvhvvvwvnpv" ]);
({ min = 5; max = 8; letter = "v" }, [ "vgvvvvvf" ]);
({ min = 6; max = 11; letter = "h" }, [ "jsngtkbnqhwthvhhhnh" ]);
({ min = 2; max = 4; letter = "j" }, [ "jjjjj" ]);
({ min = 1; max = 14; letter = "j" }, [ "jjjjjjjjjjjjjnjjj" ]);
({ min = 4; max = 7; letter = "q" }, [ "wqqxnfq" ]);
({ min = 16; max = 17; letter = "v" }, [ "vvvvvvvvvndvvvvjqv" ]);
({ min = 2; max = 6; letter = "q" }, [ "gqqqqxqqqqqqqqqq" ]);
({ min = 4; max = 7; letter = "x" }, [ "xxxjxxcxx" ]);
({ min = 2; max = 7; letter = "m" }, [ "wmhzkmj" ]);
({ min = 6; max = 10; letter = "l" }, [ "wlllnlllllllll" ]);
({ min = 7; max = 9; letter = "g" }, [ "jkzqgxgbt" ]);
({ min = 7; max = 8; letter = "f" }, [ "fffffflf" ]);
({ min = 3; max = 6; letter = "j" }, [ "dlndzj" ]);
({ min = 9; max = 12; letter = "t" }, [ "nttttttttttqt" ]);
({ min = 3; max = 8; letter = "b" }, [ "pbnlbbbbbbbbbj" ]);
({ min = 2; max = 8; letter = "t" }, [ "tjbftttdttt" ]);
({ min = 4; max = 5; letter = "j" }, [ "pvjjg" ]);
({ min = 2; max = 6; letter = "j" }, [ "jjksfb" ]);
({ min = 2; max = 4; letter = "c" }, [ "hccc" ]);
({ min = 5; max = 7; letter = "m" }, [ "mmnmmwzmmjmxb" ]);
({ min = 6; max = 8; letter = "d" }, [ "cddmkdqdkpqh" ]);
({ min = 6; max = 13; letter = "b" }, [ "ljwdhbbfzqjbz" ]);
({ min = 11; max = 16; letter = "t" }, [ "ttttttttttjctltnt" ]);
({ min = 3; max = 4; letter = "c" }, [ "xcgc" ]);
({ min = 8; max = 9; letter = "p" }, [ "gpvpdpwqpwsppppp" ]);
({ min = 3; max = 4; letter = "z" }, [ "rpvzzwfjzlkwhss" ]);
({ min = 9; max = 10; letter = "q" }, [ "jhqgdqgqrqqnqqlcqqb" ]);
({ min = 9; max = 10; letter = "z" }, [ "zzlzzpzzzsznzzz" ]);
({ min = 5; max = 7; letter = "q" }, [ "lmmqbhqzq" ]);
({ min = 9; max = 10; letter = "l" }, [ "llllllqpcqfjjcl" ]);
({ min = 12; max = 13; letter = "l" }, [ "llllllllxlllllll" ]);
({ min = 3; max = 5; letter = "z" }, [ "vzzzd" ]);
({ min = 4; max = 7; letter = "f" }, [ "ffffffvff" ]);
({ min = 7; max = 8; letter = "f" }, [ "fkffffxfmn" ]);
({ min = 14; max = 16; letter = "x" }, [ "xxxxxxxxxxxxxtxnx" ]);
({ min = 2; max = 10; letter = "t" }, [ "gcbdbqmccktp" ]);
({ min = 2; max = 3; letter = "x" }, [ "nxzd" ]);
({ min = 3; max = 8; letter = "q" }, [ "qqtxgnqgf" ]);
({ min = 9; max = 17; letter = "h" }, [ "hhhvhhhhhhqhhhphhhd" ]);
({ min = 8; max = 9; letter = "v" }, [ "vvvvvvvvvv" ]);
({ min = 2; max = 4; letter = "p" }, [ "pctjppppjbm" ]);
({ min = 2; max = 14; letter = "c" }, [ "frcgcscqgdgjzc" ]);
({ min = 2; max = 3; letter = "n" }, [ "xzjnm" ]);
({ min = 9; max = 10; letter = "k" }, [ "hkrkkkbhkkkkkk" ]);
({ min = 18; max = 19; letter = "r" }, [ "rrrrrrrrrrrrrrrrrrh" ]);
({ min = 13; max = 15; letter = "d" }, [ "bddddddmddgwddn" ]);
({ min = 1; max = 6; letter = "k" }, [ "bkhxkkkng" ]);
({ min = 8; max = 10; letter = "l" }, [ "dllmllqlmllll" ]);
({ min = 4; max = 5; letter = "q" }, [ "qqwwgqqqq" ]);
({ min = 1; max = 11; letter = "r" }, [ "jrrrcrbrksrb" ]);
({ min = 10; max = 12; letter = "w" }, [ "ggwwwwwwtrwpzlwxbww" ]);
({ min = 5; max = 6; letter = "z" }, [ "zzzkzs" ]);
({ min = 9; max = 19; letter = "z" }, [ "zgzkzzzgddzzzzzzzzzk" ]);
({ min = 18; max = 19; letter = "b" }, [ "bbbbbbbbbblbbbbbbvx" ]);
({ min = 2; max = 7; letter = "g" }, [ "jzhkwgd" ]);
({ min = 2; max = 4; letter = "j" }, [ "jjjf" ]);
({ min = 14; max = 16; letter = "m" }, [ "mmmmmmmmmmmmmmmx" ]);
({ min = 7; max = 12; letter = "c" }, [ "wfpscbrrxsssccbwg" ]);
({ min = 17; max = 18; letter = "q" }, [ "qqqxqqqqqqqqqqqqnw" ]);
({ min = 10; max = 13; letter = "k" }, [ "kkkkkkkkkckkkk" ]);
({ min = 8; max = 9; letter = "d" }, [ "dddddddtd" ]);
({ min = 7; max = 11; letter = "b" }, [ "nbbbfmbbxtbbpbxcz" ]);
({ min = 2; max = 6; letter = "p" }, [ "pppqpp" ]);
({ min = 2; max = 8; letter = "s" }, [ "shqplnxs" ]);
({ min = 3; max = 6; letter = "z" }, [ "lzzzztszsvrw" ]);
({ min = 2; max = 9; letter = "b" }, [ "sqxpbbtbbb" ]);
({ min = 2; max = 5; letter = "f" }, [ "cjffp" ]);
({ min = 2; max = 7; letter = "l" }, [ "lrllllll" ]);
({ min = 1; max = 7; letter = "t" }, [ "ttttttt" ]);
({ min = 3; max = 4; letter = "p" }, [ "vplp" ]);
({ min = 9; max = 10; letter = "p" }, [ "zppppkrpsp" ]);
({ min = 1; max = 4; letter = "j" }, [ "ttjjsqmt" ]);
({ min = 2; max = 3; letter = "c" }, [ "tmhc" ]);
({ min = 6; max = 9; letter = "h" }, [ "hhfwshhhx" ]);
({ min = 3; max = 4; letter = "p" }, [ "pppp" ]);
({ min = 3; max = 5; letter = "k" }, [ "kkkmk" ]);
({ min = 1; max = 8; letter = "d" }, [ "mddxdddxddd" ]);
({ min = 8; max = 16; letter = "w" }, [ "twwwwjkwztwwgnwwwr" ]);
({ min = 8; max = 9; letter = "p" }, [ "pppppppppp" ]);
({ min = 15; max = 17; letter = "j" }, [ "jjjjjjjkjvjjzjjlgjjj" ]);
({ min = 1; max = 8; letter = "n" }, [ "wnrnnnnnnnn" ]);
({ min = 7; max = 10; letter = "p" }, [ "pckslpcxgbpcbf" ]);
({ min = 6; max = 15; letter = "g" }, [ "mdlrdgtqgsgmdxf" ]);
({ min = 11; max = 13; letter = "x" }, [ "xxxxxxxjxxxxdxxxxx" ]);
({ min = 12; max = 14; letter = "n" }, [ "nnnnnnnnnnnmncn" ]);
({ min = 1; max = 4; letter = "x" }, [ "xbxx" ]);
({ min = 7; max = 10; letter = "k" }, [ "tdtbhkkkgh" ]);
({ min = 5; max = 8; letter = "n" }, [ "nndnqnrqnn" ]);
({ min = 5; max = 6; letter = "d" }, [ "dlnhdddd" ]);
({ min = 4; max = 8; letter = "f" }, [ "gglffnxf" ]);
({ min = 1; max = 4; letter = "g" }, [ "gddxfjtlgwzprgq" ]);
({ min = 8; max = 9; letter = "m" }, [ "mwmlwmmmlgshmmdmm" ]);
({ min = 3; max = 5; letter = "s" }, [ "zhsss" ]);
({ min = 5; max = 13; letter = "k" }, [ "nbdskzrfjxkkk" ]);
({ min = 15; max = 18; letter = "s" }, [ "sssssssssssnssssss" ]);
({ min = 6; max = 13; letter = "j" }, [ "jvrjcjjjxdgfpwjf" ]);
({ min = 4; max = 14; letter = "c" }, [ "qlslzpstbccpgrd" ]);
({ min = 4; max = 6; letter = "s" }, [ "sssstss" ]);
({ min = 5; max = 15; letter = "g" }, [ "gnjpssgpqmslgmgpx" ]);
({ min = 3; max = 5; letter = "b" }, [ "mbpcktvnb" ]);
({ min = 6; max = 7; letter = "c" }, [ "cccccrcc" ]);
({ min = 7; max = 8; letter = "t" }, [ "ttzttttt" ]);
({ min = 2; max = 3; letter = "c" }, [ "vchcd" ]);
({ min = 11; max = 14; letter = "v" }, [ "vxvvvdqvvvvqvv" ]);
({ min = 4; max = 8; letter = "q" }, [ "bqdqqqqcqq" ]);
({ min = 1; max = 3; letter = "v" }, [ "vstccv" ]);
({ min = 5; max = 9; letter = "b" }, [ "bbbbbbbbb" ]);
({ min = 10; max = 12; letter = "f" }, [ "rfsfhfgxffff" ]);
({ min = 4; max = 5; letter = "z" }, [ "zzzztp" ]);
({ min = 5; max = 6; letter = "m" }, [ "mmmmmm" ]);
({ min = 10; max = 11; letter = "m" }, [ "mmwmmmmmmmgmm" ]);
({ min = 4; max = 6; letter = "t" }, [ "xsqmqdltktvhsqsrttqk" ]);
({ min = 11; max = 13; letter = "j" }, [ "jjjjjxjjjjjjjjs" ]);
({ min = 16; max = 19; letter = "x" }, [ "xxzxxxrxxvvxdxsjkxx" ]);
({ min = 5; max = 7; letter = "t" }, [ "jmwqtpttlttlt" ]);
({ min = 7; max = 8; letter = "r" }, [ "drwfrrblrrkrrzr" ]);
({ min = 2; max = 16; letter = "z" }, [ "chbzzxwfzcfhzkzz" ]);
({ min = 9; max = 11; letter = "b" }, [ "bbbbbbhbbdb" ]);
({ min = 8; max = 10; letter = "n" }, [ "drcvnsxjfv" ]);
({ min = 3; max = 5; letter = "s" }, [ "sswsxst" ]);
({ min = 5; max = 13; letter = "v" }, [ "vvvvvvvvvvvvvvvvvv" ]);
({ min = 3; max = 4; letter = "w" }, [ "kwww" ]);
({ min = 4; max = 7; letter = "j" }, [ "zcjnjrjjjxj" ]);
({ min = 10; max = 12; letter = "c" }, [ "jwwmqvlkrqcp" ]);
({ min = 2; max = 6; letter = "r" }, [ "zrnfclvkdxtgnwprwjkp" ]);
({ min = 12; max = 13; letter = "f" }, [ "tfrfskfrsskffvfwrfpn" ]);
({ min = 18; max = 20; letter = "j" }, [ "jjjjjjljjjjjjjwjjjjr" ]);
({ min = 8; max = 13; letter = "p" }, [ "pqppqknpppmpfpshhvpp" ]);
({ min = 8; max = 19; letter = "t" }, [ "rsstqxpzttwtrbttqkgl" ]);
({ min = 3; max = 4; letter = "k" }, [ "bkkfk" ]);
({ min = 17; max = 18; letter = "s" }, [ "kqdbhcssbcsfddrpzs" ]);
({ min = 3; max = 4; letter = "r" }, [ "rqkbrrlzfcrn" ]);
({ min = 1; max = 3; letter = "t" }, [ "ttttt" ]);
({ min = 9; max = 16; letter = "f" }, [ "fdcfqmwskqvbthtf" ]);
({ min = 2; max = 3; letter = "b" }, [ "xwpbm" ]);
({ min = 12; max = 17; letter = "b" }, [ "bbwbbbthbbpxhbbbbbbb" ]);
({ min = 11; max = 20; letter = "f" }, [ "ffjvffffffzfffffffff" ]);
({ min = 1; max = 2; letter = "w" }, [ "dtwt" ]);
({ min = 4; max = 7; letter = "s" }, [ "qzpjsls" ]);
({ min = 6; max = 15; letter = "m" }, [ "mmmmmmmmmmmmmmmm" ]);
({ min = 7; max = 8; letter = "g" }, [ "ccgggggxg" ]);
({ min = 10; max = 13; letter = "m" }, [ "mxmpfcqcmqmlcmmmjcm" ]);
({ min = 6; max = 16; letter = "v" }, [ "vvvnvzwkchvpjrvv" ]);
({ min = 15; max = 16; letter = "r" }, [ "rrrrnrxnrldrprrb" ]);
({ min = 11; max = 12; letter = "p" }, [ "ppppppppppjd" ]);
({ min = 4; max = 7; letter = "z" }, [ "hzzqzzgzkz" ]);
({ min = 1; max = 14; letter = "v" }, [ "vvslgvqvvcxvvljvvtvv" ]);
({ min = 8; max = 11; letter = "w" }, [ "wtwhwfwkwwhw" ]);
({ min = 4; max = 6; letter = "c" }, [ "cccmctccdcczcccccc" ]);
({ min = 2; max = 4; letter = "z" }, [ "kzrrzt" ]);
({ min = 3; max = 4; letter = "v" }, [ "vvvmhxgs" ]);
({ min = 8; max = 12; letter = "s" }, [ "ssssssssssskssssssss" ]);
({ min = 2; max = 4; letter = "z" }, [ "rkzzz" ]);
({ min = 6; max = 7; letter = "m" }, [ "mnmrmfbmm" ]);
({ min = 8; max = 12; letter = "p" }, [ "bhphvpjfjpsj" ]);
({ min = 4; max = 6; letter = "r" }, [ "rrqrrjrr" ]);
({ min = 3; max = 6; letter = "t" }, [ "trttlth" ]);
({ min = 4; max = 5; letter = "j" }, [ "jjjvgvj" ]);
({ min = 9; max = 12; letter = "b" }, [ "lbbxlbbxkkbb" ]);
({ min = 8; max = 9; letter = "d" }, [ "dddddddwm" ]);
({ min = 14; max = 15; letter = "z" }, [ "fhdzzzgzbzvzdzz" ]);
({ min = 8; max = 11; letter = "c" }, [ "cbccrbdwdccccsk" ]);
({ min = 5; max = 6; letter = "t" }, [ "wttxtm" ]);
({ min = 11; max = 14; letter = "f" }, [ "tbvfftkfffjfffffc" ]);
({ min = 2; max = 3; letter = "b" }, [ "bbrb" ]);
({ min = 3; max = 4; letter = "b" }, [ "brbwbb" ]);
({ min = 12; max = 16; letter = "f" }, [ "ffkfffxfzfffffffl" ]);
({ min = 11; max = 14; letter = "n" }, [ "nnnnnnnqgnsnbq" ]);
({ min = 10; max = 13; letter = "z" }, [ "zzzzzzzzhzzzzzzz" ]);
({ min = 2; max = 13; letter = "z" }, [ "qzrzddvglwfzqjqln" ]);
({ min = 9; max = 12; letter = "n" }, [ "snnnnnnnnnjlnnnn" ]);
({ min = 5; max = 9; letter = "l" }, [ "pqllllbvb" ]);
({ min = 6; max = 13; letter = "z" }, [ "pszzmzzzzzzzz" ]);
({ min = 5; max = 9; letter = "d" }, [ "ddddqdddd" ]);
({ min = 9; max = 16; letter = "l" }, [ "llllllllxllllllsl" ]);
({ min = 3; max = 10; letter = "w" }, [ "rfwwgwlwftcw" ]);
({ min = 2; max = 6; letter = "t" }, [ "wphbtstkzxwntspptt" ]);
({ min = 7; max = 14; letter = "j" }, [ "xchzzxmmjkggmxqrs" ]);
({ min = 6; max = 8; letter = "f" }, [ "fffvxffffn" ]);
({ min = 13; max = 14; letter = "h" }, [ "qhhhxhrhshhhhh" ]);
({ min = 7; max = 8; letter = "q" }, [ "qqqqqqjwqqqqq" ]);
({ min = 1; max = 3; letter = "l" }, [ "llslll" ]);
({ min = 1; max = 7; letter = "f" }, [ "ftphbnfwwxzkfcsbr" ]);
({ min = 5; max = 10; letter = "r" }, [ "rjrrrrhrrxrz" ]);
({ min = 7; max = 9; letter = "p" }, [ "pppbplzph" ]);
({ min = 5; max = 7; letter = "p" }, [ "ppppptpp" ]);
({ min = 10; max = 11; letter = "t" }, [ "tttttttttjr" ]);
({ min = 12; max = 13; letter = "h" }, [ "hhhqhhhhhhbzhhhgv" ]);
({ min = 13; max = 14; letter = "z" }, [ "zzzzpzzzvzzzztz" ]);
({ min = 11; max = 12; letter = "w" }, [ "wqwrwwwkwwdw" ]);
({ min = 1; max = 11; letter = "z" }, [ "zzzzzzzzzxkzz" ]);
({ min = 1; max = 4; letter = "k" }, [ "tkks" ]);
({ min = 3; max = 6; letter = "x" }, [ "xxxtxdwxxnxxtfxxxwx" ]);
({ min = 12; max = 15; letter = "z" }, [ "kzzzzzzzzzzzzzz" ]);
({ min = 2; max = 16; letter = "k" }, [ "jdsqqmbdktthvxmk" ]);
({ min = 10; max = 13; letter = "t" }, [ "ctttfqttzqjjtfsxn" ]);
({ min = 4; max = 7; letter = "t" }, [ "tttwttk" ]);
({ min = 1; max = 4; letter = "r" }, [ "zrrprrk" ]);
({ min = 5; max = 9; letter = "v" }, [ "cvvwvkvwv" ]);
({ min = 7; max = 9; letter = "v" }, [ "vvmvvvvvlwlwgwv" ]);
({ min = 10; max = 14; letter = "q" }, [ "qqqqqqqqqqqqqq" ]);
({ min = 5; max = 6; letter = "v" }, [ "vvvvvvv" ]);
({ min = 2; max = 5; letter = "q" }, [ "cqqqqqqq" ]);
({ min = 12; max = 13; letter = "b" }, [ "bbbbgbbbbbbqvbb" ]);
({ min = 17; max = 19; letter = "v" }, [ "dhgcvvkrpwvtwvlvvrn" ]);
({ min = 4; max = 6; letter = "h" }, [ "hhmmhhh" ]);
({ min = 16; max = 18; letter = "g" }, [ "bgcggrgggggggggggg" ]);
({ min = 2; max = 4; letter = "m" }, [ "mmtv" ]);
({ min = 2; max = 9; letter = "m" }, [ "gmmmqmmlghmmkbmsqsmm" ]);
({ min = 5; max = 6; letter = "f" }, [ "lfpffffpfhf" ]);
({ min = 8; max = 9; letter = "t" }, [ "ttbttttwpt" ]);
({ min = 7; max = 9; letter = "h" }, [ "hhhhchhhch" ]);
({ min = 2; max = 4; letter = "h" }, [ "vnht" ]);
({ min = 2; max = 3; letter = "k" }, [ "kkkqk" ]);
({ min = 11; max = 13; letter = "c" }, [ "fhfcjkwxcfsqctc" ]);
({ min = 10; max = 11; letter = "c" }, [ "cscscccvcqx" ]);
({ min = 7; max = 9; letter = "k" }, [ "vkkkkxrlck" ]);
({ min = 6; max = 7; letter = "g" }, [ "fgfrgdtkrmsgsg" ]);
({ min = 6; max = 7; letter = "r" }, [ "rjrrkrrrt" ]);
({ min = 8; max = 13; letter = "t" }, [ "ttttttttttttt" ]);
({ min = 2; max = 9; letter = "g" }, [ "mnkkxttgm" ]);
({ min = 3; max = 6; letter = "g" }, [ "ggggqlgng" ]);
({ min = 5; max = 6; letter = "j" }, [ "lgjxgj" ]);
({ min = 5; max = 7; letter = "n" }, [ "ngkknrh" ]);
({ min = 1; max = 16; letter = "d" }, [ "dddddddddddddddpd" ]);
({ min = 1; max = 7; letter = "p" }, [ "plpdvrswpppzcp" ]);
({ min = 5; max = 14; letter = "n" }, [ "znnnlntlnnnnnjn" ]);
({ min = 10; max = 11; letter = "w" }, [ "wwwwmwqwwwgwv" ]);
({ min = 7; max = 8; letter = "g" }, [ "gggggggt" ]);
({ min = 3; max = 8; letter = "m" }, [ "mmtmmdkncmqxqrz" ]);
({ min = 3; max = 5; letter = "t" }, [ "ttrtnt" ]);
({ min = 2; max = 3; letter = "z" }, [ "pdzscxrmmbpchktnz" ]);
({ min = 1; max = 5; letter = "z" }, [ "ztcgllzvzzvcfvgzr" ]);
({ min = 6; max = 8; letter = "l" }, [ "llltpkdsdkltcll" ]);
({ min = 10; max = 13; letter = "h" }, [ "fhhhhcbhhchxc" ]);
({ min = 14; max = 15; letter = "x" }, [ "xcpnxnxvbxxxbxx" ]);
({ min = 9; max = 16; letter = "n" }, [ "nnnnnnnnwpncdnnnnn" ]);
({ min = 4; max = 8; letter = "t" }, [ "ttbtwxlxhtkzwrrvc" ]);
({ min = 4; max = 10; letter = "l" }, [ "lwzglkgkcwzwb" ]);
({ min = 2; max = 3; letter = "t" }, [ "lstmttkttt" ]);
({ min = 4; max = 6; letter = "f" }, [ "fhsfdf" ]);
({ min = 4; max = 5; letter = "n" }, [ "nnnbns" ]);
({ min = 1; max = 10; letter = "v" }, [ "tvvvvvvvvw" ]);
({ min = 2; max = 13; letter = "r" }, [ "rtrrgqprprrbfrrrrr" ]);
({ min = 7; max = 8; letter = "d" }, [ "cfwtbdsd" ]);
({ min = 4; max = 5; letter = "g" }, [ "ggwgggg" ]);
({ min = 4; max = 11; letter = "r" }, [ "nccrrfcxsnvkrrrz" ]);
({ min = 1; max = 5; letter = "b" }, [ "bcqwbzbbtfbwb" ]);
({ min = 8; max = 16; letter = "d" }, [ "dfddddddddzdddddtdd" ]);
({ min = 3; max = 7; letter = "g" }, [ "ggggbdgggg" ]);
({ min = 8; max = 9; letter = "r" }, [ "rlrrrrrrd" ]);
({ min = 4; max = 5; letter = "n" }, [ "ntnnsn" ]);
({ min = 7; max = 8; letter = "z" }, [ "zzzzzzww" ]);
({ min = 1; max = 13; letter = "p" }, [ "vpppppnfcppqm" ]);
({ min = 4; max = 8; letter = "s" }, [ "sqssxsss" ]);
({ min = 2; max = 4; letter = "z" }, [ "bznt" ]);
({ min = 5; max = 6; letter = "w" }, [ "swczww" ]);
({ min = 3; max = 7; letter = "l" }, [ "lbvklllknbh" ]);
({ min = 3; max = 4; letter = "g" }, [ "gbvgj" ]);
({ min = 9; max = 10; letter = "w" }, [ "wwwwwwwtwwbww" ]);
({ min = 7; max = 9; letter = "k" }, [ "pvkqmkkqllxktdz" ]);
({ min = 6; max = 7; letter = "g" }, [ "bkgkggsfbgh" ]);
({ min = 12; max = 13; letter = "j" }, [ "xjlxjkjjcdjbj" ]);
({ min = 6; max = 7; letter = "x" }, [ "xxxxvgl" ]);
({ min = 5; max = 13; letter = "x" }, [ "xpsxxtbbxfxwmnmbr" ]);
({ min = 2; max = 4; letter = "g" }, [ "pxzp" ]);
({ min = 4; max = 5; letter = "k" }, [ "qskgkkktfk" ]);
({ min = 10; max = 12; letter = "w" }, [ "wjjxwswnwqwkdmbwwww" ]);
({ min = 1; max = 9; letter = "m" }, [ "kmmlcmdbh" ]);
({ min = 3; max = 6; letter = "x" }, [ "mxzqrlxn" ]);
({ min = 4; max = 8; letter = "k" }, [ "rkrcltmkk" ]);
({ min = 6; max = 11; letter = "x" }, [ "xfrjffvwdrpvkvgjz" ]);
({ min = 9; max = 10; letter = "g" }, [ "ggggpkgcggggsg" ]);
({ min = 7; max = 9; letter = "j" }, [ "jjgmjjbjjjj" ]);
({ min = 2; max = 10; letter = "r" }, [ "rjrrrrrrrlr" ]);
({ min = 4; max = 5; letter = "x" }, [ "xxxlxd" ]);
({ min = 3; max = 4; letter = "r" }, [ "rzrnrr" ]);
({ min = 1; max = 4; letter = "s" }, [ "ssslsssssss" ]);
({ min = 2; max = 5; letter = "n" }, [ "nbtdnf" ]);
({ min = 3; max = 5; letter = "l" }, [ "lfvlll" ]);
({ min = 3; max = 9; letter = "z" }, [ "kcrjrrjvzjnzbvx" ]);
({ min = 7; max = 12; letter = "j" }, [ "jjjjjjjjjjjmjj" ]);
({ min = 4; max = 9; letter = "c" }, [ "cccgccxqm" ]);
({ min = 2; max = 8; letter = "h" }, [ "pxjwgjlpd" ]);
({ min = 13; max = 17; letter = "k" }, [ "kkkkkkkkkkkkvkkktkk" ]);
({ min = 8; max = 13; letter = "j" }, [ "xjxjgjjjjjjlx" ]);
({ min = 1; max = 5; letter = "t" }, [ "ttttg" ]);
({ min = 2; max = 5; letter = "w" }, [ "wdwwfwwxwwnwwwwck" ]);
({ min = 12; max = 13; letter = "n" }, [ "nnnnnnnnnnvpqnn" ]);
({ min = 1; max = 3; letter = "g" }, [ "gtgg" ]);
({ min = 5; max = 7; letter = "d" }, [ "dqddldd" ]);
({ min = 4; max = 14; letter = "n" }, [ "hnxfhwfkvvnqzhz" ]);
({ min = 17; max = 19; letter = "z" }, [ "zcdpstnzvtpvmzzcztz" ]);
({ min = 5; max = 6; letter = "b" }, [ "bbmbbdcbw" ]);
({ min = 4; max = 13; letter = "t" }, [ "ttqttcttqtfttttt" ]);
({ min = 1; max = 2; letter = "l" }, [ "lvlklkc" ]);
({ min = 11; max = 16; letter = "d" }, [ "ddddcdtdddwdrdddddd" ]);
({ min = 3; max = 5; letter = "j" }, [ "xcnjfznjhqzj" ]);
({ min = 1; max = 3; letter = "s" }, [ "ssssssrps" ]);
({ min = 2; max = 4; letter = "w" }, [ "twzfrrgqjkcczjp" ]);
({ min = 11; max = 12; letter = "d" }, [ "qvddkhdddskn" ]);
({ min = 4; max = 5; letter = "t" }, [ "wtjttt" ]);
({ min = 1; max = 4; letter = "t" }, [ "ptttldt" ]);
({ min = 6; max = 8; letter = "z" }, [ "zzzzzdzhz" ]);
({ min = 4; max = 8; letter = "d" }, [ "ddkdddddddd" ]);
({ min = 6; max = 7; letter = "f" }, [ "ffffpfff" ]);
({ min = 7; max = 9; letter = "j" }, [ "hzjspbsvjfgjtwd" ]);
({ min = 3; max = 6; letter = "b" }, [ "bblkbb" ]);
({ min = 6; max = 16; letter = "m" }, [ "qcmbmrsxdfwxmmnf" ]);
({ min = 3; max = 4; letter = "l" }, [ "lxll" ]);
({ min = 1; max = 5; letter = "g" }, [ "gzmgslb" ]);
({ min = 8; max = 17; letter = "z" }, [ "mpxpgljtpcjwltmst" ]);
({ min = 5; max = 8; letter = "b" }, [ "bbwbbbnkmbmhgbsbj" ]);
({ min = 2; max = 3; letter = "f" }, [ "ffff" ]);
({ min = 6; max = 9; letter = "s" }, [ "sssfsgsgsbstscdszhr" ]);
({ min = 1; max = 2; letter = "v" }, [ "bsjwvvv" ]);
({ min = 1; max = 2; letter = "k" }, [ "kkkkkkkkkkkkkkk" ]);
({ min = 1; max = 2; letter = "h" }, [ "stxx" ]);
({ min = 8; max = 9; letter = "k" }, [ "kgkckkwcmmtkkwkw" ]);
({ min = 1; max = 18; letter = "p" }, [ "pppprzcpgpmpnppppz" ]);
({ min = 1; max = 5; letter = "x" }, [ "qtcrxsggtdl" ]);
({ min = 14; max = 17; letter = "j" }, [ "pwfvwzjsmtqcsjjgd" ]);
({ min = 1; max = 13; letter = "w" }, [ "wxbvsfwfwkxwwvzg" ]);
({ min = 2; max = 6; letter = "k" }, [ "kwqkkkk" ]);
({ min = 2; max = 7; letter = "t" }, [ "vrztkcx" ]);
({ min = 9; max = 13; letter = "k" }, [ "kkkkkkkrkkkkt" ]);
({ min = 1; max = 2; letter = "w" }, [ "wrjxrxwwp" ]);
({ min = 3; max = 11; letter = "m" }, [ "mwmjmmmmqlmmmmml" ]);
({ min = 1; max = 6; letter = "c" }, [ "zcccccc" ]);
({ min = 9; max = 10; letter = "h" }, [ "vhhntcjjhr" ]);
({ min = 18; max = 19; letter = "z" }, [ "wszdzfjnvzbhrgzcgrz" ]);
({ min = 11; max = 12; letter = "x" }, [ "xxxxxxxxxxwxxx" ]);
({ min = 7; max = 11; letter = "b" }, [ "bvnbbbbjbnbbb" ]);
({ min = 4; max = 14; letter = "r" }, [ "fbrxgtsbrbrrhrrdrr" ]);
({ min = 15; max = 16; letter = "x" }, [ "xxxxxwxktpxkvxxb" ]);
({ min = 1; max = 3; letter = "r" }, [ "rtqvmrzwnwmgxk" ]);
({ min = 12; max = 14; letter = "s" }, [ "sfssfspctrskspqssss" ]);
({ min = 5; max = 7; letter = "p" }, [ "ppppgppp" ]);
({ min = 4; max = 5; letter = "n" }, [ "nnhtd" ]);
({ min = 7; max = 14; letter = "d" }, [ "ddddddswbddmdr" ]);
({ min = 5; max = 6; letter = "j" }, [ "jjjjjj" ]);
({ min = 8; max = 12; letter = "d" }, [ "zjdddddddtnvdvdddd" ]);
({ min = 1; max = 4; letter = "q" }, [ "rhrz" ]);
({ min = 3; max = 17; letter = "c" }, [ "lnspcskfnggtqvxfc" ]);
({ min = 7; max = 9; letter = "d" }, [ "lvxrkdwfc" ]);
({ min = 5; max = 6; letter = "g" }, [ "gxggfkjmh" ]);
({ min = 2; max = 7; letter = "k" }, [ "jkpsbcq" ]);
({ min = 4; max = 12; letter = "p" }, [ "pcptjwccgppcp" ]);
({ min = 5; max = 16; letter = "w" }, [ "wwwwwwwwwwwwwwwwww" ]);
({ min = 3; max = 6; letter = "f" }, [ "rcmcffxgm" ]);
({ min = 10; max = 14; letter = "t" }, [ "tttttmtwxwthvd" ]);
({ min = 9; max = 11; letter = "v" }, [ "vvvvvvvvvvvvvv" ]);
({ min = 4; max = 7; letter = "x" }, [ "xxclxxx" ]);
({ min = 7; max = 10; letter = "z" }, [ "zzzzzvzzmnz" ]);
({ min = 2; max = 3; letter = "t" }, [ "hcthfjxkh" ]);
({ min = 1; max = 2; letter = "v" }, [ "xvxvk" ]);
({ min = 8; max = 11; letter = "b" }, [ "bbbbbbblbbnb" ]);
({ min = 6; max = 18; letter = "b" }, [ "qbjxjbrqfrwgdrzldbt" ]);
({ min = 14; max = 16; letter = "v" }, [ "sprsxwphvvbvcvkv" ]);
({ min = 11; max = 17; letter = "c" }, [ "dpwlccccmclbqzcptrc" ]);
({ min = 13; max = 19; letter = "w" }, [ "wwwwwkwwwwwmwwzmvww" ]);
({ min = 8; max = 14; letter = "m" }, [ "mdfvmpmrskvqcmvmddv" ]);
({ min = 4; max = 13; letter = "v" }, [ "vvvvvvvvvgkvvvvvwvv" ]);
({ min = 1; max = 3; letter = "g" }, [ "lgwqgg" ]);
({ min = 1; max = 3; letter = "r" }, [ "rrrrzrr" ]);
({ min = 2; max = 4; letter = "h" }, [ "hkrh" ]);
({ min = 3; max = 6; letter = "h" }, [ "gwhzvhv" ]);
({ min = 16; max = 19; letter = "x" }, [ "xxxxxxxxxxlxvxxxnxt" ]);
({ min = 2; max = 3; letter = "t" }, [ "tttq" ]);
({ min = 9; max = 13; letter = "w" }, [ "zkmpkfwwpwwwcw" ]);
({ min = 7; max = 8; letter = "n" }, [ "nnjxnnnnbnhnr" ]);
({ min = 5; max = 7; letter = "w" }, [ "ddzdwwww" ]);
({ min = 11; max = 16; letter = "b" }, [ "bbbbbbbbbbbbbbbbb" ]);
({ min = 2; max = 9; letter = "j" }, [ "wvtdzjzzb" ]);
({ min = 3; max = 4; letter = "d" }, [ "dcbdd" ]);
({ min = 3; max = 4; letter = "n" }, [ "gnng" ]);
({ min = 5; max = 11; letter = "c" }, [ "fgcqcnqbcmv" ]);
({ min = 13; max = 16; letter = "s" }, [ "nqssssssssxssskss" ]);
({ min = 6; max = 9; letter = "b" }, [ "bjrdbbprb" ]);
({ min = 3; max = 14; letter = "d" }, [ "jtdbmqqxsndhcdgm" ]);
({ min = 10; max = 11; letter = "p" }, [ "pppppxpppxbp" ]);
({ min = 8; max = 9; letter = "x" }, [ "xxdpxxhxq" ]);
({ min = 5; max = 7; letter = "f" }, [ "ffffdkf" ]);
({ min = 2; max = 9; letter = "z" }, [ "zzzzzjzdnzzzzzc" ]);
({ min = 6; max = 11; letter = "h" }, [ "hhhhhhhhhhhhhhh" ]);
({ min = 4; max = 7; letter = "v" }, [ "vvvsrqn" ]);
({ min = 4; max = 6; letter = "j" }, [ "pgphwjwxj" ]);
({ min = 14; max = 15; letter = "q" }, [ "qqqqqqqqlqqqqsq" ]);
({ min = 9; max = 10; letter = "t" }, [ "ttttnpxtttttt" ]);
({ min = 3; max = 4; letter = "m" }, [ "dwkj" ]);
({ min = 7; max = 11; letter = "m" }, [ "mmtmcmmjdpggmm" ]);
({ min = 1; max = 7; letter = "c" }, [ "crccnlrccccs" ]);
({ min = 3; max = 7; letter = "m" }, [ "llczxdkcmpsbmpbmnmv" ]);
({ min = 2; max = 4; letter = "k" }, [ "fckkfbkjd" ]);
({ min = 15; max = 16; letter = "p" }, [ "hnhwphpktppppppp" ]);
({ min = 12; max = 15; letter = "g" }, [ "ghggvzgqhkbgtgg" ]);
({ min = 1; max = 3; letter = "q" }, [ "kqqqktqq" ]);
({ min = 1; max = 8; letter = "g" }, [ "bzsjpsrshsggdm" ]);
({ min = 1; max = 6; letter = "b" }, [ "bbbbbbbbb" ]);
({ min = 17; max = 18; letter = "v" }, [ "hfvqvvvvvbvvfvvbgs" ]);
({ min = 2; max = 11; letter = "z" }, [ "wzcxsgzkqfzx" ]);
({ min = 8; max = 11; letter = "z" }, [ "fbxzgzlzzzzpzzvkzz" ]);
({ min = 2; max = 15; letter = "g" }, [ "hghsqkjhjgldscgh" ]);
({ min = 5; max = 7; letter = "p" }, [ "pvfppppp" ]);
({ min = 4; max = 14; letter = "z" }, [ "zzzzvzmzkrzrzmpzzfz" ]);
({ min = 1; max = 4; letter = "z" }, [ "zzfzzzljzzgznzzzzz" ]);
({ min = 5; max = 6; letter = "c" }, [ "cccccc" ]);
({ min = 5; max = 11; letter = "j" }, [ "jkmrhqksjpj" ]);
({ min = 1; max = 3; letter = "f" }, [ "bffwff" ]);
({ min = 4; max = 8; letter = "d" }, [ "dtddzddmd" ]);
({ min = 14; max = 15; letter = "s" }, [ "sswsssssssssstqs" ]);
({ min = 6; max = 18; letter = "j" }, [ "tjjzhmbsxgkwcdmjpj" ]);
({ min = 2; max = 6; letter = "d" }, [ "qddddwqcdjldbsd" ]);
({ min = 9; max = 11; letter = "t" }, [ "wqtjttwtctttttt" ]);
({ min = 7; max = 8; letter = "q" }, [ "rqvhnctm" ]);
({ min = 7; max = 10; letter = "k" }, [ "nqkdtkrkwvkkkbpk" ]);
({ min = 5; max = 6; letter = "l" }, [ "lllllm" ]);
({ min = 2; max = 4; letter = "p" }, [ "pkpp" ]);
({ min = 3; max = 6; letter = "t" }, [ "psxjqth" ]);
({ min = 1; max = 2; letter = "j" }, [ "xjwnjff" ]);
({ min = 3; max = 4; letter = "t" }, [ "zqbt" ]);
({ min = 3; max = 7; letter = "c" }, [ "qcccgwg" ]);
({ min = 3; max = 5; letter = "q" }, [ "ncqqlt" ]);
({ min = 1; max = 4; letter = "h" }, [ "fhhh" ]);
({ min = 6; max = 8; letter = "x" }, [ "xxxxxxxxxx" ]);
({ min = 1; max = 5; letter = "r" }, [ "rrnkrrpfrdfrxx" ]);
({ min = 3; max = 6; letter = "b" }, [ "lcbbbqmhbbsb" ]);
({ min = 6; max = 15; letter = "z" }, [ "qtfvzzskzsdqqxdkzsh" ]);
({ min = 9; max = 11; letter = "d" }, [ "qpcdwddqxdzf" ]);
({ min = 4; max = 5; letter = "k" }, [ "kkkkkwznkgtk" ]);
({ min = 3; max = 4; letter = "h" }, [ "hhdgr" ]);
({ min = 10; max = 12; letter = "p" }, [ "ppcppplppppz" ]);
({ min = 3; max = 9; letter = "g" }, [ "xcgnnhstgtmqdpwghx" ]);
({ min = 3; max = 4; letter = "g" }, [ "gggg" ]);
({ min = 10; max = 12; letter = "k" }, [ "krkkwglqbrkkkkkkj" ]);
({ min = 11; max = 14; letter = "d" }, [ "dddddddvdddddvw" ]);
({ min = 4; max = 12; letter = "k" }, [ "khlkkksjkkkktkk" ]);
({ min = 15; max = 17; letter = "p" }, [ "xsmppwpxxqpwtdjhj" ]);
({ min = 6; max = 11; letter = "q" }, [ "dtxjgzcpxcmqhlnqr" ]);
({ min = 9; max = 11; letter = "q" }, [ "qqqqqqqqjtr" ]);
({ min = 10; max = 14; letter = "h" }, [ "hgphhzqkvhsjhhn" ]);
({ min = 12; max = 16; letter = "v" }, [ "ghvvvmhvvvvvvsvbvv" ]);
({ min = 1; max = 2; letter = "x" }, [ "xqxx" ]);
({ min = 7; max = 9; letter = "r" }, [ "rbrrrrrjd" ]);
({ min = 15; max = 16; letter = "z" }, [ "zzzzzzzzzzzczzmxz" ]);
({ min = 11; max = 12; letter = "x" }, [ "xxjxkxkxxxrdbxxxkx" ]);
({ min = 6; max = 7; letter = "b" }, [ "gbrbbwc" ]);
({ min = 8; max = 9; letter = "q" }, [ "qqqqqqqdq" ]);
({ min = 10; max = 16; letter = "x" }, [ "fxxxxxxxxxxxxxxrx" ]);
({ min = 5; max = 8; letter = "f" }, [ "fffffhsdmgfb" ]);
({ min = 6; max = 7; letter = "p" }, [ "kppcppt" ]);
({ min = 8; max = 9; letter = "z" }, [ "zqzzzzzbczrz" ]);
({ min = 13; max = 14; letter = "d" }, [ "ddhvvdzddddcdcddddd" ]);
({ min = 5; max = 8; letter = "p" }, [ "ppppppph" ]);
({ min = 8; max = 10; letter = "b" }, [ "bbbbbbbjbb" ]);
({ min = 1; max = 6; letter = "j" }, [ "xjcwnnwwmgjcxfkpw" ]);
({ min = 11; max = 18; letter = "v" }, [ "vvvvvvvvvvxvvvvvvvv" ]);
({ min = 4; max = 5; letter = "j" }, [ "xjjls" ]);
({ min = 1; max = 4; letter = "g" }, [ "ggggg" ]);
({ min = 2; max = 4; letter = "m" }, [ "cmmm" ]);
({ min = 1; max = 7; letter = "g" }, [ "gkggnwg" ]);
({ min = 7; max = 10; letter = "t" }, [ "btwtgttrmwtptmt" ]);
({ min = 6; max = 7; letter = "t" }, [ "vtzltkt" ]);
({ min = 4; max = 7; letter = "n" }, [ "jcdnbwnn" ]);
({ min = 8; max = 14; letter = "f" }, [ "fffvblcftkfqff" ]);
({ min = 4; max = 10; letter = "c" }, [ "ccccccmccc" ]);
({ min = 1; max = 6; letter = "l" }, [ "lllljgllll" ]);
({ min = 1; max = 6; letter = "w" }, [ "hkwdfpkp" ]);
({ min = 6; max = 10; letter = "n" }, [ "znnnctlnklnnnrn" ]);
({ min = 3; max = 6; letter = "s" }, [ "fsshbttssbqb" ]);
({ min = 11; max = 14; letter = "s" }, [ "sszsssslssmssq" ]);
({ min = 1; max = 4; letter = "l" }, [ "jwll" ]);
({ min = 1; max = 11; letter = "b" }, [ "bbblbbbbbbbb" ]);
({ min = 11; max = 12; letter = "p" }, [ "ppppppppppdvppppppp" ]);
({ min = 8; max = 11; letter = "q" }, [ "pqjqwqqwqqck" ]);
({ min = 9; max = 11; letter = "r" }, [ "rrrrrrrrfrrrr" ]);
({ min = 4; max = 8; letter = "p" }, [ "pppfpppb" ]);
({ min = 15; max = 16; letter = "g" }, [ "ggggggggggggkggg" ]);
({ min = 1; max = 11; letter = "m" }, [ "fzpmdrbhlbmmshk" ]);
({ min = 3; max = 17; letter = "j" }, [ "pmpjvtlxhlpcwphpjjb" ]);
({ min = 3; max = 4; letter = "s" }, [ "cjsz" ]);
({ min = 10; max = 11; letter = "z" }, [ "zzzzzvzzzzzz" ]);
({ min = 1; max = 2; letter = "p" }, [ "bvppp" ]);
({ min = 5; max = 9; letter = "w" }, [ "wgjbwwwwl" ]);
({ min = 19; max = 20; letter = "r" }, [ "rrrrrrrrrrrrrrrrrrdm" ]);
({ min = 4; max = 6; letter = "q" }, [ "fqqjbqqq" ]);
({ min = 2; max = 11; letter = "s" }, [ "wplxcnbdspsdlh" ]);
({ min = 3; max = 13; letter = "d" }, [ "dlvmkddkgdrfgxhxldg" ]);
({ min = 4; max = 8; letter = "d" }, [ "wsdrldsdwzdddjnfh" ]);
({ min = 1; max = 10; letter = "j" }, [ "tjbwrzcnlcjjhkrsjjj" ]);
({ min = 2; max = 3; letter = "m" }, [ "mmwm" ]);
({ min = 7; max = 10; letter = "b" }, [ "lcbbbbcrfb" ]);
({ min = 18; max = 19; letter = "c" }, [ "cgcccrccfccccccccnc" ]);
({ min = 3; max = 5; letter = "j" }, [ "jjpjfjjjj" ]);
({ min = 13; max = 14; letter = "c" }, [ "cckccccccccccccc" ]);
({ min = 4; max = 9; letter = "q" }, [ "qqqqqqqqlqqq" ]);
({ min = 3; max = 6; letter = "w" }, [ "twwvwwhmkwnjnss" ]);
({ min = 8; max = 9; letter = "p" }, [ "pppzpppzcp" ]);
({ min = 9; max = 10; letter = "l" }, [ "lllslvllwc" ]);
({ min = 4; max = 7; letter = "m" }, [ "sjmhhhmr" ]);
({ min = 1; max = 4; letter = "v" }, [ "vvrnvvtv" ]);
({ min = 1; max = 4; letter = "r" }, [ "rmrrrcrrcprrrrbnq" ]);
({ min = 7; max = 11; letter = "g" }, [ "ggghggggkgdwg" ]);
({ min = 2; max = 12; letter = "l" }, [ "lmlllllllllllll" ]);
({ min = 1; max = 3; letter = "q" }, [ "qqqq" ]);
({ min = 11; max = 12; letter = "x" }, [ "xxxxxdxpmxxxx" ]);
({ min = 2; max = 3; letter = "q" }, [ "qqqh" ]);
({ min = 5; max = 6; letter = "w" }, [ "wwkwwwwgb" ]);
({ min = 10; max = 11; letter = "w" }, [ "wwhwwwvwwwwww" ]);
({ min = 1; max = 12; letter = "c" }, [ "vcccccccccccc" ]);
({ min = 1; max = 3; letter = "r" }, [ "rrlrr" ]);
({ min = 7; max = 12; letter = "w" }, [ "qpfcxwnvwzhwzvgwpwcm" ]);
({ min = 7; max = 9; letter = "g" }, [ "ggggggfglg" ]);
({ min = 2; max = 6; letter = "j" }, [ "cjqzkl" ]);
({ min = 5; max = 7; letter = "w" }, [ "wwrwmwww" ]);
({ min = 6; max = 12; letter = "q" }, [ "shqjlnqgkfcq" ]);
({ min = 11; max = 12; letter = "d" }, [ "gddddddcddxd" ]);
({ min = 3; max = 5; letter = "x" }, [ "xxxxh" ]);
({ min = 6; max = 12; letter = "b" }, [ "bpmbmbbbbrdb" ]);
({ min = 5; max = 7; letter = "j" }, [ "jtxbwtj" ]);
({ min = 5; max = 6; letter = "t" }, [ "tntttv" ]);
({ min = 17; max = 18; letter = "t" }, [ "tttttttttttttwttsms" ]);
({ min = 7; max = 15; letter = "g" }, [ "gcghxlgzlgrdklnggtl" ]);
({ min = 7; max = 12; letter = "s" }, [ "sdpssdwmssssgs" ]);
({ min = 7; max = 15; letter = "k" }, [ "kkpmsskkpzhkbdkksk" ]);
({ min = 5; max = 6; letter = "b" }, [ "bbbbbb" ]);
({ min = 4; max = 7; letter = "c" }, [ "hxccchdrf" ]);
({ min = 3; max = 6; letter = "z" }, [ "vzhzcpz" ]);
({ min = 14; max = 16; letter = "q" }, [ "ppqrgfqvpmfjwqqz" ]);
({ min = 5; max = 6; letter = "g" }, [ "svgwgxrnmngrgg" ]);
({ min = 2; max = 13; letter = "p" }, [ "spkpvpdlcprlb" ]);
({ min = 1; max = 4; letter = "v" }, [ "vvvv" ]);
({ min = 10; max = 11; letter = "r" }, [ "xrjcrrrrwrrh" ]);
({ min = 6; max = 11; letter = "h" }, [ "hhhvchbhhhphhhhh" ]);
({ min = 7; max = 9; letter = "w" }, [ "wjwwwwzzwwkwbkrl" ]);
({ min = 7; max = 11; letter = "x" }, [ "xxdxcllxxxr" ]);
({ min = 6; max = 17; letter = "b" }, [ "brthfqjmbbbbbfljbfb" ]);
({ min = 11; max = 12; letter = "l" }, [ "lclllzllllxjdl" ]);
({ min = 7; max = 12; letter = "g" }, [ "gchglbgmggbqvg" ]);
({ min = 2; max = 5; letter = "z" }, [ "zzpjzqzz" ]);
({ min = 7; max = 16; letter = "h" }, [ "hhhqndzhnhpzhqhhmhb" ]);
({ min = 16; max = 17; letter = "k" }, [ "kkkkzkkkkkkwfkkckkkk" ]);
({ min = 2; max = 3; letter = "v" }, [ "vgbvv" ]);
({ min = 9; max = 10; letter = "j" }, [ "jjjjjjjjjv" ]);
({ min = 1; max = 4; letter = "n" }, [ "cnxn" ]);
({ min = 7; max = 10; letter = "s" }, [ "sssssssssssstsbss" ]);
({ min = 3; max = 12; letter = "s" }, [ "ksjbxfhcsswstr" ]);
({ min = 4; max = 15; letter = "z" }, [ "zpzmzzpzzzzzzzmzz" ]);
({ min = 9; max = 12; letter = "q" }, [ "qpqwgcrqpqdp" ]);
({ min = 2; max = 11; letter = "b" }, [ "bbhnnbdblgbbbbbgb" ]);
({ min = 5; max = 6; letter = "h" }, [ "hhhhhhhhhhh" ]);
({ min = 8; max = 12; letter = "t" }, [ "ttttttbttttttt" ]);
({ min = 17; max = 18; letter = "t" }, [ "qtkstpsdthtvtftgthqw" ]);
({ min = 2; max = 6; letter = "l" }, [ "lplllll" ]);
({ min = 6; max = 7; letter = "c" }, [ "zmqjcbmrcclzcc" ]);
({ min = 5; max = 16; letter = "z" }, [ "zzzzdzzzzzzzjzztm" ]);
({ min = 10; max = 14; letter = "b" }, [ "qrjbbbbblkrbbwbbrcbc" ]);
({ min = 2; max = 15; letter = "d" }, [ "dgdddddddwddddzdd" ]);
({ min = 3; max = 8; letter = "p" }, [ "zmmhfppjlrwnpp" ]);
({ min = 17; max = 19; letter = "r" }, [ "krrhprxrrrnfqkslrpr" ]);
({ min = 7; max = 10; letter = "n" }, [ "nnnnnnnnsqn" ]);
({ min = 3; max = 10; letter = "g" }, [ "gggggzggghjgggwggg" ]);
({ min = 1; max = 4; letter = "x" }, [ "sxxxxf" ]);
({ min = 6; max = 11; letter = "j" }, [ "jjljjtjjjjj" ]);
({ min = 7; max = 8; letter = "f" }, [ "fffffffff" ]);
({ min = 2; max = 3; letter = "n" }, [ "knndzlnhz" ]);
({ min = 4; max = 7; letter = "l" }, [ "lgplzpkjq" ]);
({ min = 9; max = 10; letter = "z" }, [ "zzzzzzzbszzr" ]);
({ min = 8; max = 13; letter = "h" }, [ "kdhhhhhhhkrgchhh" ]);
({ min = 1; max = 3; letter = "d" }, [ "lddrbd" ]);
({ min = 18; max = 19; letter = "n" }, [ "nnnnnnnnnxnnnnnnnns" ]);
({ min = 4; max = 7; letter = "x" }, [ "gxxqxxxxjcxl" ]);
({ min = 5; max = 13; letter = "r" }, [ "trfthwjntjlwn" ]);
({ min = 8; max = 12; letter = "d" }, [ "xxpdjrdwbddddmdqvd" ]);
({ min = 5; max = 6; letter = "j" }, [ "jxjjvkd" ]);
({ min = 4; max = 5; letter = "b" }, [ "dmwpbbx" ]);
({ min = 3; max = 4; letter = "w" }, [ "wjwl" ]);
({ min = 2; max = 3; letter = "s" }, [ "ssjss" ]);
({ min = 1; max = 6; letter = "m" }, [ "mhkmsthqxrxmqq" ]);
({ min = 7; max = 16; letter = "c" }, [ "vzqnsscchrkjpckwn" ]);
({ min = 10; max = 15; letter = "p" }, [ "ppfzpdppppzpphsvpppq" ]);
({ min = 3; max = 19; letter = "g" }, [ "lgxbrcsvgzrncgzggdgg" ]);
({ min = 10; max = 14; letter = "h" }, [ "phtzhhhhhhhhkjn" ]);
({ min = 15; max = 16; letter = "f" }, [ "ffffffffffffffdff" ]);
({ min = 1; max = 4; letter = "x" }, [ "xmlxjlblpctjxzmdgkgr" ]);
({ min = 10; max = 12; letter = "c" }, [ "cpcccccccwcdcccccccv" ]);
({ min = 3; max = 4; letter = "t" }, [ "tstt" ]);
({ min = 10; max = 14; letter = "s" }, [ "ssldsssssrshsvs" ]);
({ min = 7; max = 9; letter = "v" }, [ "vvvvvvvvv" ]);
({ min = 11; max = 13; letter = "c" }, [ "cccfcccctccksczcct" ]);
({ min = 5; max = 6; letter = "c" }, [ "zcccrl" ]);
({ min = 12; max = 18; letter = "p" }, [ "ppgppdlgqpkpwppkphp" ]);
({ min = 10; max = 16; letter = "v" }, [ "vvvvvvvvvvvvmvvhvv" ]);
({ min = 15; max = 16; letter = "n" }, [ "nnnnnnnnnnnnnntnnn" ]);
({ min = 15; max = 18; letter = "t" }, [ "tzktddqnhtjtsktlkt" ]);
({ min = 4; max = 7; letter = "q" }, [ "mqjbqkqlqnqqdxn" ]);
({ min = 1; max = 3; letter = "x" }, [ "fxcx" ]);
({ min = 5; max = 6; letter = "c" }, [ "jwccccxggtgcc" ]);
({ min = 8; max = 16; letter = "m" }, [ "mpjmmpmmmmmcmmmc" ]);
({ min = 7; max = 9; letter = "q" }, [ "qrwpkjfqzq" ]);
({ min = 1; max = 12; letter = "g" }, [ "bsqgxbjngmgw" ]);
({ min = 4; max = 5; letter = "w" }, [ "wwwbw" ]);
({ min = 14; max = 17; letter = "r" }, [ "nvmrhtqdkqkbcrzjp" ]);
({ min = 12; max = 14; letter = "b" }, [ "bbbbmbbbbvbrpr" ]);
({ min = 7; max = 8; letter = "s" }, [ "ksssssls" ]);
({ min = 10; max = 12; letter = "m" }, [ "mmmjmwcfmbmmmmmm" ]);
({ min = 3; max = 4; letter = "q" }, [ "cqqq" ]);
({ min = 7; max = 9; letter = "d" }, [ "dvhqpbrjz" ]);
({ min = 4; max = 5; letter = "b" }, [ "bhbxbzbm" ]);
({ min = 10; max = 12; letter = "l" }, [ "llglllplhcgmtxlln" ]);
({ min = 7; max = 12; letter = "j" }, [ "rpwrjzjxktwjjjr" ]);
({ min = 12; max = 14; letter = "m" }, [ "cdmdcmkzknfmgwvwxfm" ]);
({ min = 1; max = 2; letter = "b" }, [ "bbnt" ]);
({ min = 4; max = 16; letter = "h" }, [ "hlhhdhhzhhmlzqhhh" ]);
({ min = 2; max = 5; letter = "d" }, [ "dqfwxgccx" ]);
({ min = 4; max = 8; letter = "q" }, [ "vnqtwhsqvsnqw" ]);
({ min = 7; max = 8; letter = "k" }, [ "kxhkpssk" ]);
({ min = 8; max = 13; letter = "t" }, [ "zbmtwrbcvhfhj" ]);
({ min = 5; max = 8; letter = "s" }, [ "hshrssvclnjs" ]);
({ min = 10; max = 12; letter = "v" }, [ "rfwhbpxrtwpvvt" ]);
({ min = 3; max = 6; letter = "l" }, [ "lllstl" ]);
({ min = 5; max = 6; letter = "d" }, [ "dfhbxq" ]);
({ min = 7; max = 12; letter = "d" }, [ "dwldsrddmnddv" ]);
({ min = 7; max = 8; letter = "v" }, [ "vvljwkvh" ]);
({ min = 3; max = 4; letter = "b" }, [ "wdkhbbrxmcr" ]);
({ min = 3; max = 5; letter = "v" }, [ "cvvvvvvhr" ]);
({ min = 5; max = 7; letter = "h" }, [ "hjhhqhzhhw" ]);
({ min = 3; max = 5; letter = "n" }, [ "nlndkn" ]);
({ min = 6; max = 8; letter = "t" }, [ "zttttjtml" ]);
({ min = 1; max = 7; letter = "r" }, [ "trrrrrrrrr" ]);
({ min = 3; max = 6; letter = "h" }, [ "nzgxhzhftnhchhvt" ]);
({ min = 8; max = 9; letter = "s" }, [ "ksssssfsrss" ]);
({ min = 1; max = 6; letter = "l" }, [ "lqxqgw" ]);
({ min = 18; max = 20; letter = "v" }, [ "vvvvgvvvvvvvvvvvvlvq" ]);
({ min = 2; max = 4; letter = "q" }, [ "bqlqhxqxrxqqzq" ]);
({ min = 3; max = 18; letter = "s" }, [ "fdcswgtgxlhfvzzznssh" ]);
({ min = 9; max = 10; letter = "p" }, [ "ppzsplppppvhppd" ]);
({ min = 12; max = 13; letter = "b" }, [ "bbbbbbbbbbbbbb" ]);
({ min = 12; max = 16; letter = "w" }, [ "wwwwwwtwwwwwwwwrwww" ]);
({ min = 6; max = 8; letter = "h" }, [ "hhmhhxhbf" ]);
({ min = 1; max = 7; letter = "c" }, [ "gpdvnmnbgtlnwlzx" ]);
({ min = 3; max = 4; letter = "z" }, [ "zzzz" ]);
({ min = 4; max = 9; letter = "m" }, [ "gzmmdcdgm" ]);
({ min = 2; max = 4; letter = "t" }, [ "vgsttn" ]);
({ min = 1; max = 8; letter = "r" }, [ "rrlrrrlr" ]);
({ min = 5; max = 10; letter = "w" }, [ "rrwgwfrprpvww" ]);
({ min = 1; max = 4; letter = "r" }, [ "srrr" ]);
({ min = 11; max = 12; letter = "r" }, [ "rrrrrrrrrrbrrrrrr" ]);
({ min = 3; max = 5; letter = "c" }, [ "gqvcnctdwn" ]);
({ min = 4; max = 6; letter = "p" }, [ "mrmvppprp" ]);
({ min = 2; max = 3; letter = "l" }, [ "kxlvdtgtlhsrl" ]);
({ min = 8; max = 16; letter = "h" }, [ "chchhhhzfhshhhhhh" ]);
({ min = 6; max = 8; letter = "r" }, [ "rrrrrfrrr" ]);
({ min = 17; max = 20; letter = "t" }, [ "ptttttttnttttktttttt" ]);
({ min = 6; max = 9; letter = "r" }, [ "zbltfrdlrwrj" ]);
({ min = 6; max = 7; letter = "d" }, [ "gqhsddd" ]);
({ min = 3; max = 5; letter = "j" }, [ "mtlhj" ]);
({ min = 17; max = 18; letter = "x" }, [ "xpxxxxxxxxxxxxxxnxxt" ]);
({ min = 7; max = 11; letter = "n" }, [ "vpnnnncnnbnn" ]);
({ min = 3; max = 4; letter = "q" }, [ "cqlhqqqsvfz" ]);
({ min = 8; max = 12; letter = "f" }, [ "ffffffffffff" ]);
({ min = 3; max = 4; letter = "g" }, [ "fbksgtqggggggrgg" ]);
({ min = 7; max = 9; letter = "d" }, [ "skntdmdcdzmnhvdj" ]);
({ min = 7; max = 10; letter = "w" }, [ "wwwwwwbwww" ]);
({ min = 10; max = 11; letter = "r" }, [ "fgpkwrwqgbs" ]);
({ min = 9; max = 10; letter = "h" }, [ "hhhjchhhnhh" ]);
({ min = 11; max = 12; letter = "d" }, [ "ddddddddddkh" ]);
({ min = 9; max = 11; letter = "c" }, [ "czzjtllcdff" ]);
({ min = 1; max = 2; letter = "t" }, [ "rrxdnzf" ]);
({ min = 3; max = 4; letter = "n" }, [ "nndk" ]);
({ min = 1; max = 2; letter = "m" }, [ "qqvmh" ]);
({ min = 7; max = 9; letter = "x" }, [ "qbxkxxxxxxx" ]);
({ min = 3; max = 13; letter = "v" }, [ "vgvtfmtwnrfbxvtkt" ]);
({ min = 2; max = 5; letter = "j" }, [ "tjpjjrlxzjjcsl" ]);
({ min = 13; max = 14; letter = "z" }, [ "zzzzzzzzzzzzbz" ]);
({ min = 2; max = 4; letter = "w" }, [ "wwww" ]);
({ min = 5; max = 6; letter = "x" }, [ "xxxxxdx" ]);
({ min = 2; max = 12; letter = "p" }, [ "pppppfpvfppcmpd" ]);
({ min = 4; max = 5; letter = "r" }, [ "rrrwfrr" ]);
({ min = 3; max = 8; letter = "c" }, [ "cwmnsxcccjd" ]);
({ min = 2; max = 4; letter = "j" }, [ "jjhh" ]);
({ min = 4; max = 6; letter = "j" }, [ "ljtjvzqzktz" ]);
({ min = 7; max = 9; letter = "v" }, [ "vvvvvvcfv" ]);
({ min = 4; max = 6; letter = "m" }, [ "mmrmxmkmmmgr" ]);
({ min = 6; max = 10; letter = "p" }, [ "cpkppbppppqplxr" ]);
({ min = 12; max = 20; letter = "b" }, [ "stbmpspzhplbhhtbpbwb" ]);
({ min = 5; max = 7; letter = "w" }, [ "wkwwbwhwwrwwswwwz" ]);
({ min = 2; max = 7; letter = "r" }, [ "rssttxm" ]);
({ min = 3; max = 8; letter = "r" }, [ "gcrtpkrrdrk" ]);
({ min = 7; max = 9; letter = "d" }, [ "ddddddbdgsd" ]);
({ min = 8; max = 10; letter = "w" }, [ "wwwwwwwwwwwww" ]);
({ min = 11; max = 15; letter = "g" }, [ "ggdtgggrgggghggg" ]);
({ min = 7; max = 14; letter = "j" }, [ "nfvlqhccbnwkjhwnnvt" ]);
({ min = 14; max = 15; letter = "g" }, [ "gggggggggggggfg" ]);
({ min = 14; max = 15; letter = "h" }, [ "hhhhhhhhhhhhhhv" ]);
({ min = 5; max = 6; letter = "c" }, [ "pmccjccccrcc" ]);
({ min = 2; max = 7; letter = "n" }, [ "vzxncpcdqzvqgmf" ]);
({ min = 1; max = 4; letter = "z" }, [ "xzzz" ]);
({ min = 7; max = 10; letter = "d" }, [ "ddddddnddd" ]);
({ min = 5; max = 6; letter = "q" }, [ "qqqqqbq" ]);
({ min = 4; max = 18; letter = "v" }, [ "vvvjvwvvcvrvnvwhvv" ]);
({ min = 5; max = 6; letter = "x" }, [ "xxxxxb" ]);
({ min = 3; max = 8; letter = "n" }, [ "jnnnnnnnz" ]);
({ min = 4; max = 5; letter = "g" }, [ "ggngg" ]);
({ min = 9; max = 11; letter = "f" }, [ "bsffzfvfffxffjtf" ]);
({ min = 7; max = 9; letter = "p" }, [ "pzpppppppprp" ]);
({ min = 1; max = 4; letter = "x" }, [ "sxcxfx" ]);
({ min = 4; max = 12; letter = "h" }, [ "hhtphhhhdhkghhchhvqh" ]);
({ min = 9; max = 10; letter = "s" }, [ "ssssslsssss" ]);
({ min = 1; max = 5; letter = "t" }, [ "ttttw" ]);
({ min = 4; max = 7; letter = "b" }, [ "xbxxvxbdpnflzbzz" ]);
({ min = 11; max = 12; letter = "f" }, [ "ndppjfvgbvndzgflkwj" ]);
({ min = 4; max = 5; letter = "v" }, [ "vdlvk" ]);
({ min = 6; max = 7; letter = "q" }, [ "qqvqbqqqql" ]);
({ min = 3; max = 7; letter = "r" }, [ "rrjrrrr" ]);
({ min = 7; max = 10; letter = "h" }, [ "dhhhqhbcbhhlhhhhf" ]);
({ min = 12; max = 14; letter = "c" }, [ "cgccccccccccczcjc" ]);
({ min = 1; max = 2; letter = "f" }, [ "fsff" ]);
({ min = 7; max = 8; letter = "b" }, [ "cqqbcwxzbb" ]);
({ min = 8; max = 13; letter = "j" }, [ "jjsgdjjnjxcjzjj" ]);
({ min = 5; max = 7; letter = "h" }, [ "hhchhph" ]);
({ min = 3; max = 6; letter = "g" }, [ "ggggglg" ]);
({ min = 6; max = 9; letter = "g" }, [ "zgmsgggglwgngn" ]);
({ min = 1; max = 6; letter = "s" }, [ "kmsssssspssss" ]);
({ min = 1; max = 5; letter = "q" }, [ "fscrqmqqpqqqdtgspmc" ]);
({ min = 5; max = 8; letter = "f" }, [ "cftsfmwff" ]);
({ min = 2; max = 15; letter = "g" }, [ "cgvtzfjsxfgdsggtk" ]);
({ min = 3; max = 4; letter = "v" }, [ "vhdq" ]);
({ min = 13; max = 14; letter = "c" }, [ "ccccccccccccns" ]);
({ min = 2; max = 4; letter = "z" }, [ "zzzz" ]);
({ min = 2; max = 3; letter = "c" }, [ "ccscc" ]);
({ min = 2; max = 12; letter = "x" }, [ "bxxxqxrbxlxlbxxvxxv" ]);
({ min = 5; max = 9; letter = "f" }, [ "ffffsfmffxfff" ]);
({ min = 10; max = 12; letter = "m" }, [ "wnmmjmmmmmmlmwk" ]);
({ min = 1; max = 4; letter = "t" }, [ "ltttttt" ]);
({ min = 2; max = 10; letter = "r" }, [ "brlrmrrrrljrvr" ]);
({ min = 9; max = 13; letter = "p" }, [ "ppppmtppnpptps" ]);
({ min = 3; max = 7; letter = "p" }, [ "pjppppppppbpw" ]);
({ min = 7; max = 11; letter = "f" }, [ "fzfzzvfvfcfkffffffk" ]);
({ min = 4; max = 6; letter = "p" }, [ "pppppp" ]);
({ min = 5; max = 6; letter = "b" }, [ "blbbbb" ]);
({ min = 9; max = 13; letter = "w" }, [ "wwzwwbwcnwtzw" ]);
({ min = 4; max = 8; letter = "d" }, [ "jkjrdxnv" ]);
({ min = 5; max = 7; letter = "t" }, [ "ttkttrsttppmttgtqtf" ]);
({ min = 3; max = 4; letter = "n" }, [ "ngmn" ]);
({ min = 5; max = 7; letter = "h" }, [ "cvhdhvbhnknhh" ]);
({ min = 12; max = 13; letter = "m" }, [ "vsqmvcvgmmhmk" ]);
({ min = 9; max = 10; letter = "s" }, [ "ssssssssts" ]);
({ min = 5; max = 14; letter = "p" }, [ "pppkdppppbppppppppp" ]);
({ min = 2; max = 4; letter = "v" }, [ "lbbcvhxgns" ]);
({ min = 1; max = 4; letter = "g" }, [ "zggg" ]);
({ min = 2; max = 3; letter = "f" }, [ "wgnf" ]);
({ min = 6; max = 7; letter = "j" }, [ "jwcjjfjjjjxzjbjk" ]);
({ min = 3; max = 15; letter = "b" }, [ "tvlrztbfwfbwqknhzbw" ]);
({ min = 10; max = 11; letter = "g" }, [ "ggggggggfxk" ]);
({ min = 2; max = 3; letter = "q" }, [ "vqqq" ]);
({ min = 10; max = 13; letter = "v" }, [ "fcvvrvsvdvjvwvvxvgt" ]);
({ min = 3; max = 4; letter = "c" }, [ "mcmc" ]);
({ min = 3; max = 9; letter = "d" }, [ "xddqhsthcdpfsdd" ]);
({ min = 11; max = 12; letter = "n" }, [ "mnnnlnncnkng" ]);
({ min = 2; max = 10; letter = "p" }, [ "pppgpgppxmp" ]);
({ min = 1; max = 4; letter = "s" }, [ "hswssss" ]);
({ min = 3; max = 4; letter = "h" }, [ "zhhh" ]);
({ min = 4; max = 8; letter = "f" }, [ "jnwfmtfjf" ]);
({ min = 18; max = 19; letter = "z" }, [ "zzzzzzzzzzzzzzzzzzz" ]);
({ min = 13; max = 16; letter = "h" }, [ "rghbhvnfsnvvhjjhd" ]);
({ min = 7; max = 11; letter = "d" }, [ "cdddrddncffsdlddl" ]);
({ min = 4; max = 5; letter = "w" }, [ "dgwnw" ]);
({ min = 15; max = 17; letter = "v" }, [ "vvvvkvvvvfvqvvqvnnvv" ]);
({ min = 6; max = 11; letter = "v" }, [ "gvvvvvvvvvv" ]);
({ min = 18; max = 19; letter = "n" }, [ "nnnnnnnnnnnnnnnnnnn" ]);
({ min = 4; max = 6; letter = "k" }, [ "tkkkzklkvbkk" ]);
({ min = 12; max = 16; letter = "n" }, [ "nnnnnnnnnnnwnnnhnnn" ]);
({ min = 2; max = 3; letter = "w" }, [ "wwww" ]);
({ min = 13; max = 14; letter = "t" }, [ "tttttttttttttb" ]);
({ min = 6; max = 9; letter = "j" }, [ "zjjjjfjjmjjj" ]);
({ min = 3; max = 9; letter = "j" }, [ "ltljpknxj" ]);
({ min = 4; max = 10; letter = "m" }, [ "wzmrmsvfwrjds" ]);
({ min = 6; max = 9; letter = "s" }, [ "ssssszssw" ]);
({ min = 4; max = 5; letter = "v" }, [ "vdnvvbbzvdns" ]);
({ min = 4; max = 20; letter = "d" }, [ "ddvdvrpddwdddddtdsdd" ]);
({ min = 1; max = 2; letter = "h" }, [ "jhvhh" ]);
({ min = 2; max = 3; letter = "n" }, [ "ncnt" ]);
({ min = 4; max = 6; letter = "f" }, [ "ffwfpff" ]);
({ min = 3; max = 4; letter = "w" }, [ "gtwzxkw" ]);
({ min = 8; max = 14; letter = "x" }, [ "twxqvxxgcpnxxxcwxx" ]);
({ min = 1; max = 13; letter = "v" }, [ "tvvvvvvcvvvvvplvvvtv" ]);
({ min = 7; max = 14; letter = "c" }, [ "rmrzctbccfcnrcdsdc" ]);
({ min = 4; max = 8; letter = "b" }, [ "bbbbbbbjb" ]);
({ min = 5; max = 15; letter = "t" }, [ "vzttttbtdttfqstt" ]);
({ min = 9; max = 17; letter = "f" }, [ "fffpfffbfffxtfdfpf" ]);
({ min = 7; max = 12; letter = "n" }, [ "nnnlnrccnnnld" ]);
({ min = 3; max = 5; letter = "g" }, [ "ggpgrg" ]);
({ min = 4; max = 5; letter = "w" }, [ "wknhd" ]);
({ min = 9; max = 10; letter = "f" }, [ "wfbfrfffqkfkfg" ]);
({ min = 3; max = 4; letter = "g" }, [ "ggtg" ]);
({ min = 2; max = 5; letter = "c" }, [ "cqzdfcccpcc" ]);
({ min = 2; max = 11; letter = "z" }, [ "gzhzxhdttbz" ]);
({ min = 5; max = 7; letter = "j" }, [ "pxjjzwjskjjtj" ]);
({ min = 4; max = 5; letter = "t" }, [ "ttttts" ]);
({ min = 8; max = 9; letter = "c" }, [ "cqccjccqcc" ]);
({ min = 6; max = 7; letter = "z" }, [ "zzzvzmvzl" ]);
({ min = 9; max = 12; letter = "d" }, [ "ddddddddjdddd" ]);
({ min = 5; max = 7; letter = "c" }, [ "xtcxfncccxkjzcckcbtr" ]);
({ min = 2; max = 3; letter = "w" }, [ "twknmwb" ]);
({ min = 4; max = 6; letter = "w" }, [ "wwcwwbwqhqkw" ]);
({ min = 3; max = 10; letter = "x" }, [ "xxxxxxxxxxxxxxxxxx" ]);
({ min = 1; max = 7; letter = "p" }, [ "pppppppp" ]);
({ min = 4; max = 5; letter = "n" }, [ "rnnnf" ]);
({ min = 5; max = 7; letter = "f" }, [ "fftfjhffff" ]);
({ min = 8; max = 10; letter = "h" }, [ "hhthhhhhhzh" ]);
({ min = 7; max = 17; letter = "s" }, [ "spsssshssslsssljs" ]);
({ min = 4; max = 8; letter = "l" }, [ "jlltllllll" ]);
({ min = 6; max = 7; letter = "j" }, [ "jjjjjwc" ]);
({ min = 2; max = 18; letter = "f" }, [ "rftjffffqvffsffpfc" ]);
({ min = 16; max = 17; letter = "w" }, [ "wwwwwwwwwwwwwwdsjwp" ]);
({ min = 2; max = 3; letter = "p" }, [ "qtxp" ]);
({ min = 15; max = 17; letter = "s" }, [ "sssssssjssssssksc" ]);
({ min = 2; max = 5; letter = "v" }, [ "xvvcvmvv" ]);
({ min = 3; max = 13; letter = "x" }, [ "xxvxxsxxxmhjtx" ]);
({ min = 6; max = 17; letter = "z" }, [ "kfzvzzmrndrbqwskz" ]);
({ min = 2; max = 3; letter = "s" }, [ "xsjqx" ]);
({ min = 17; max = 18; letter = "t" }, [ "tttttttttttttrtjmstj" ]);
({ min = 1; max = 3; letter = "n" }, [ "nnjnn" ]);
({ min = 3; max = 7; letter = "c" }, [ "lcxcvccpc" ]);
({ min = 7; max = 10; letter = "l" }, [ "lllcllllll" ]);
({ min = 1; max = 3; letter = "z" }, [ "zhwzjzjzw" ]);
({ min = 8; max = 9; letter = "d" }, [ "gvfbkdtdn" ]);
({ min = 5; max = 7; letter = "w" }, [ "smwwgfwwww" ]);
({ min = 4; max = 9; letter = "n" }, [ "nclnmsnntrkmdvw" ]);
({ min = 1; max = 5; letter = "g" }, [ "qfgggg" ]);
({ min = 8; max = 10; letter = "w" }, [ "wwkwwbwwww" ]);
({ min = 1; max = 4; letter = "g" }, [ "kfjrxgtzcwgvrqg" ]);
({ min = 2; max = 9; letter = "f" }, [ "fffffffffff" ]);
({ min = 8; max = 10; letter = "l" }, [ "llllllltlnl" ]);
({ min = 2; max = 13; letter = "c" }, [ "cwcccccvccccccdccc" ]);
({ min = 2; max = 5; letter = "c" }, [ "wczcs" ]);
({ min = 2; max = 4; letter = "c" }, [ "mlxc" ]);
({ min = 3; max = 4; letter = "v" }, [ "vlvvjx" ]);
({ min = 12; max = 14; letter = "h" }, [ "hshhhhhhhhhhhhh" ]);
({ min = 17; max = 19; letter = "f" }, [ "fffffffvffffffffffff" ]);
({ min = 1; max = 7; letter = "m" }, [ "xfbmltbm" ]);
({ min = 8; max = 15; letter = "c" }, [ "ccdcngpcqjscmcqk" ]);
({ min = 6; max = 11; letter = "b" }, [ "fbbbbxbbbbdnj" ]);
({ min = 13; max = 14; letter = "j" }, [ "nxjmwtqpjwzjcj" ]);
({ min = 1; max = 4; letter = "t" }, [ "nttjt" ]);
({ min = 11; max = 13; letter = "m" }, [ "mmmmmgmmmmnmw" ]);
({ min = 6; max = 14; letter = "h" }, [ "zwfvkhhhhpghrh" ]);
({ min = 8; max = 9; letter = "l" }, [ "nfllllllrhlllv" ]);
({ min = 6; max = 10; letter = "q" }, [ "nqpqpqqqqs" ]);
({ min = 4; max = 7; letter = "n" }, [ "nnnjhng" ]);
({ min = 7; max = 11; letter = "s" }, [ "ssklvwsnssstxsbsr" ]);
({ min = 3; max = 12; letter = "x" }, [ "hxxxxxxxxxxxxbjrc" ]);
({ min = 3; max = 18; letter = "k" }, [ "kktkkkkkkxbkckcpkk" ]);
({ min = 1; max = 9; letter = "x" }, [ "lxscklsfxwxbj" ]);
({ min = 3; max = 4; letter = "z" }, [ "zkfm" ]);
({ min = 3; max = 15; letter = "r" }, [ "rgrshbkrwmrdkrwsjdr" ]);
({ min = 4; max = 5; letter = "r" }, [ "mrlrv" ]);
({ min = 3; max = 4; letter = "h" }, [ "hhhg" ]);
({ min = 2; max = 7; letter = "t" }, [ "hthhzbztttbt" ]);
({ min = 4; max = 7; letter = "r" }, [ "rrxxrnr" ]);
({ min = 1; max = 4; letter = "z" }, [ "zdzz" ]);
({ min = 2; max = 4; letter = "g" }, [ "rgggggctgcjljg" ]);
({ min = 12; max = 13; letter = "z" }, [ "hjtcmnbczznzz" ]);
({ min = 1; max = 4; letter = "s" }, [ "ssstss" ]);
({ min = 10; max = 11; letter = "q" }, [ "qwqqqqrqqtrq" ]);
({ min = 7; max = 18; letter = "z" }, [ "hhzjjrzzgjhxznmkzzhz" ]);
({ min = 4; max = 5; letter = "f" }, [ "flfffd" ]);
({ min = 6; max = 9; letter = "c" }, [ "cdccccccwcc" ]);
({ min = 2; max = 6; letter = "r" }, [ "rfrrrpr" ]);
({ min = 4; max = 10; letter = "x" }, [ "xxzqxxqqxxbn" ]);
({ min = 2; max = 6; letter = "m" }, [ "dsxdmmbm" ]);
({ min = 3; max = 4; letter = "q" }, [ "kqqkhn" ]);
({ min = 7; max = 10; letter = "w" }, [ "wwwwwswlwgrw" ]);
({ min = 5; max = 11; letter = "t" }, [ "tlxljjtntmt" ]);
({ min = 7; max = 11; letter = "v" }, [ "vcvvvvgvvvmvvvv" ]);
({ min = 2; max = 4; letter = "m" }, [ "mmmcmd" ]);
({ min = 10; max = 12; letter = "x" }, [ "lxxxxsxzxqxb" ]);
({ min = 4; max = 8; letter = "g" }, [ "vmsgpdvgwldrgvw" ]);
({ min = 11; max = 13; letter = "t" }, [ "ktnttftxvtttttttqqtt" ]);
({ min = 5; max = 6; letter = "m" }, [ "mmztdm" ]);
({ min = 2; max = 5; letter = "p" }, [ "ppppdp" ]);
({ min = 1; max = 12; letter = "d" }, [ "dddddddddddcdddd" ]);
({ min = 8; max = 10; letter = "h" }, [ "thzhmhhqhrhhhhm" ]);
({ min = 2; max = 10; letter = "d" }, [ "dddddmdddpdd" ]);
({ min = 8; max = 13; letter = "q" }, [ "qjqqrwzpqnqgkfqqhqq" ]);
({ min = 1; max = 5; letter = "t" }, [ "thptxlqtt" ]);
({ min = 4; max = 6; letter = "v" }, [ "nqdfvvhvvvbv" ]);
({ min = 8; max = 12; letter = "j" }, [ "jjjjjkpmjjjj" ]);
({ min = 10; max = 12; letter = "p" }, [ "pfpgtppppgpxpgsjpp" ]);
({ min = 2; max = 6; letter = "v" }, [ "zvvtcdbbxv" ]);
({ min = 8; max = 10; letter = "b" }, [ "qbmbrbsbbb" ]);
({ min = 2; max = 3; letter = "r" }, [ "drxr" ]);
({ min = 6; max = 7; letter = "s" }, [ "ssssssv" ]);
({ min = 1; max = 2; letter = "v" }, [ "jpvvv" ]);
({ min = 12; max = 14; letter = "r" }, [ "rrrrrrjrvrrtrr" ]);
({ min = 6; max = 7; letter = "r" }, [ "rjscgmr" ]);
({ min = 11; max = 15; letter = "l" }, [ "llhlllbkllnlzlwllp" ]);
({ min = 6; max = 10; letter = "l" }, [ "lcqlllblll" ]);
({ min = 3; max = 5; letter = "c" }, [ "drfvccctgv" ]);
({ min = 1; max = 3; letter = "s" }, [ "sssss" ]);
({ min = 1; max = 4; letter = "b" }, [ "btbsjnx" ]);
({ min = 9; max = 12; letter = "b" }, [ "bbnbbbbbbbbbb" ]);
({ min = 2; max = 3; letter = "n" }, [ "nknn" ]);
({ min = 2; max = 8; letter = "f" }, [ "ffffffnffs" ]);
({ min = 4; max = 7; letter = "w" }, [ "wwwcwww" ]);
({ min = 7; max = 14; letter = "f" }, [ "mfbpkxjzprnmpnfhdb" ]);
({ min = 4; max = 11; letter = "h" }, [ "hhhshhhhhhhhhhhhhh" ]);
({ min = 6; max = 8; letter = "q" }, [ "hhqdrqbmvlgpwkppc" ]);
({ min = 1; max = 4; letter = "v" }, [ "vvvv" ]);
({ min = 6; max = 18; letter = "v" }, [ "vvvvvdvvvvvvvvvvvgvv" ]);
({ min = 1; max = 3; letter = "b" }, [ "bbxcbbbbt" ]);
({ min = 4; max = 5; letter = "t" }, [ "tlvhq" ]);
({ min = 3; max = 8; letter = "n" }, [ "njnzfnnns" ]);
({ min = 5; max = 9; letter = "z" }, [ "zpczdfwzzcxncdzz" ]);
({ min = 9; max = 14; letter = "s" }, [ "qdsssfrszsrsts" ]);
({ min = 4; max = 8; letter = "q" }, [ "jqpmbzjqb" ]);
({ min = 7; max = 13; letter = "w" }, [ "wjwwwwwwwwxwfww" ]);
({ min = 16; max = 19; letter = "g" }, [ "zggggggggggggggqggfg" ]);
({ min = 3; max = 8; letter = "s" }, [ "gsgsdtdsf" ]);
({ min = 2; max = 14; letter = "k" }, [ "rkbbpnpvjkvcskdx" ]);
({ min = 4; max = 6; letter = "s" }, [ "xssjzvs" ]);
({ min = 15; max = 16; letter = "j" }, [ "cgdjjjjfjdwjjjjs" ]);
({ min = 3; max = 6; letter = "n" }, [ "nsnnnx" ]);
({ min = 9; max = 10; letter = "x" }, [ "xxxxxpxxxmxx" ]);
({ min = 7; max = 8; letter = "x" }, [ "xxxxxxxl" ]);
({ min = 2; max = 9; letter = "t" }, [ "tjttttttmtttt" ]);
({ min = 12; max = 14; letter = "g" }, [ "gggggggggsgqzgj" ]);
({ min = 17; max = 19; letter = "c" }, [ "ccccccccccccccccccc" ]);
({ min = 8; max = 11; letter = "p" }, [ "pppppppcpppppp" ]);
({ min = 12; max = 15; letter = "s" }, [ "sssssksssssssss" ]);
({ min = 11; max = 12; letter = "k" }, [ "krkkkfkktkkq" ]);
({ min = 17; max = 18; letter = "q" }, [ "qqgtqwqqqqqqqqqqjqqq" ]);
({ min = 5; max = 16; letter = "g" }, [ "ngrwggggkzbggjtg" ]);
({ min = 12; max = 15; letter = "v" }, [ "wvvvpvvvsvvvvdpvv" ]);
({ min = 5; max = 6; letter = "f" }, [ "ffqfnffffffhf" ]);
({ min = 7; max = 8; letter = "n" }, [ "nbnnnncm" ]);
({ min = 4; max = 7; letter = "p" }, [ "mppppvvrp" ]);
({ min = 10; max = 11; letter = "b" }, [ "qbbbbbszbnbkbb" ]);
({ min = 8; max = 10; letter = "j" }, [ "jjjzjjjfjj" ]);
({ min = 3; max = 7; letter = "c" }, [ "cvtcccnkcvmccgccchc" ]);
({ min = 2; max = 3; letter = "w" }, [ "wnzd" ]);
({ min = 9; max = 10; letter = "d" }, [ "dddddddddwdldmdddddd" ]);
({ min = 4; max = 6; letter = "b" }, [ "bbbdbtbb" ]);
({ min = 2; max = 10; letter = "h" }, [ "hhhhhhhhhhh" ]);
({ min = 10; max = 15; letter = "g" }, [ "ggckgcgmcgjgggglgdgk" ]);
({ min = 3; max = 4; letter = "k" }, [ "kkgkkk" ]);
({ min = 10; max = 12; letter = "k" }, [ "sfbkhzjmbkfqrtrrq" ]);
({ min = 3; max = 9; letter = "g" }, [ "ggbgsgvggs" ]);
({ min = 2; max = 7; letter = "s" }, [ "sshpxwjbhmz" ]);
({ min = 6; max = 7; letter = "x" }, [ "xftzsrx" ]);
({ min = 1; max = 6; letter = "k" }, [ "rkkkkck" ]);
({ min = 4; max = 7; letter = "p" }, [ "pppjprjp" ]);
({ min = 17; max = 18; letter = "j" }, [ "jjjjjjjjwjjjzkjjjrh" ]);
({ min = 3; max = 9; letter = "p" }, [ "xjpptspczfx" ]);
({ min = 12; max = 15; letter = "m" }, [ "mmmmmmmmmmmmmmx" ]);
({ min = 8; max = 9; letter = "t" }, [ "zttttttsldtxr" ]);
({ min = 4; max = 7; letter = "z" }, [ "zzzzzzzz" ]);
({ min = 15; max = 18; letter = "v" }, [ "vvvvvvvvvvkvvvfvvzv" ]);
({ min = 2; max = 8; letter = "q" }, [ "vmhvmqdqz" ]);
({ min = 15; max = 18; letter = "m" }, [ "mhmmmmpmmmsmmnbmwmm" ]);
({ min = 7; max = 16; letter = "h" }, [ "hmgwvnnvhhzfhlhqhh" ]);
({ min = 6; max = 7; letter = "f" }, [ "pqpfjmhqk" ]);
({ min = 4; max = 11; letter = "p" }, [ "jzpprcpcmmpb" ]);
({ min = 9; max = 10; letter = "f" }, [ "fffpffffjff" ]);
({ min = 1; max = 5; letter = "d" }, [ "ndtdc" ]);
  ] in

  [
    Some (count_all count_min_max data);
    Some (count_all is_in_position data);
 ]
