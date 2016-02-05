module internal Ploeh.Numsense.SwissGerman

open Ploeh.Numsense.InternalDsl


let rec internal positive x =

    let ones x = 
        match x % 10 with
        |  0 -> ""
        |  1 -> "eis"
        |  2 -> "zwöi"
        |  3 -> "drü"
        |  4 -> "vier"
        |  5 -> "föif"
        |  6 -> "sächs"
        |  7 -> "sibe"
        |  8 -> "acht"
        |  9 -> "nün"

    let onesAsTensPrefix x =
        match x % 10 with
        | 0 -> ""
        | 1 -> "eine"
        | 2 -> "zwöie"
        | 3 -> "drüe"
        | 4 -> "viere"
        | 5 -> "föife"
        | 6 -> "sächse"
        | 7 -> "sibene"
        | 8 -> "achte"
        | 9 -> "nüne"

    let tens x =
        match x % 100 with
        | Between 0 10 x -> ones x
        | 10 -> "zäh"
        | 11 -> "öuf"
        | 12 -> "zwöuf"
        | 13 -> "drizäh"
        | 14 -> "vierzäh"
        | 15 -> "füfzäh"
        | 16 -> "sächzäh"
        | 17 -> "sibezäh"
        | 18 -> "achtzäh"
        | 19 -> "nünzäh"
        | Between 20 30 x -> (onesAsTensPrefix x) + "zwänzg"
        | Between 30 40 x -> (onesAsTensPrefix x) + "drissg"
        | Between 40 50 x -> (onesAsTensPrefix x) + "vierzg"
        | Between 50 60 x -> (onesAsTensPrefix x) + "füfzg"
        | Between 60 70 x -> (onesAsTensPrefix x) + "sächzg"
        | Between 70 80 x -> (onesAsTensPrefix x) + "sibezg"
        | 80 -> "achtzg"
        | Between 81 90 x -> (onesAsTensPrefix x) + "nachtzg"
        | Between 90 100 x -> (onesAsTensPrefix x) + "nünzg"

    let simplePrefix x name suffix =
        match x with
        | 0 -> suffix
        | 1 -> name + suffix
        | _ -> (positive x) + name + suffix

    let hunderts x =
        simplePrefix (x / 100) "hundert" (tens x)

    let hundertsPostfix hasPrefix x =
        match x % 1000 with
        | 0 -> ""
        | Between 0 100 x -> (if hasPrefix then "und" else "") + (tens x)
        | Between 100 200 x -> "ei" + (hunderts x)
        | x -> hunderts x

    let thousands x =
        simplePrefix (x / 1000) "tusig" (hundertsPostfix (x >= 1000) x)

    let big factor (name: string) x =
        let plural = if name.EndsWith "e" then name else name + "e"
        let start = 
            if (x / factor) = 1
            then "ei " + name
            else (positive (x / factor)) + " " + plural
        let rest = positive (x % factor)
        if (rest = "") then start else start + " " + rest


    match x with
    | x when x <= 0 -> ""
    | Between 0 100 x -> tens x
    | Between 100 1000 x -> hunderts x
    | Between 1000 1000000 x -> thousands x
    | Between 1000000 1000000000 x -> big 1000000 "Million" x
    | _ -> big 1000000000 "Milliarde" x

let internal toBerneseImp x =
    match x with
    | x when x < 0 -> (positive -x) + " unger null"
    | 0 -> "null"
    | _ -> positive x


let internal tryParseBerneseImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                      -> Some acc

        | StartsWith "zäh"      t -> conv         (10  + acc) t
        | StartsWith "öuf"      t -> conv         (11  + acc) t
        | StartsWith "zwöuf"    t -> conv         (12  + acc) t
        | StartsWith "drizäh"   t -> conv         (13  + acc) t
        | StartsWith "vierzäh"  t -> conv         (14  + acc) t
        | StartsWith "füfzäh"   t -> conv         (15  + acc) t
        | StartsWith "sächzäh"  t -> conv         (16  + acc) t
        | StartsWith "sibezäh"  t -> conv         (17  + acc) t
        | StartsWith "achtzäh"  t -> conv         (18  + acc) t
        | StartsWith "nünzäh"   t -> conv         (19  + acc) t

        | StartsWith "eine"     t -> conv          (1  + acc) t
        | StartsWith "zwöie"    t -> conv          (2  + acc) t
        | StartsWith "drüe"     t -> conv          (3  + acc) t
        | StartsWith "viere"    t -> conv          (4  + acc) t
        | StartsWith "föife"    t -> conv          (5  + acc) t
        | StartsWith "sächse"   t -> conv          (6  + acc) t
        | StartsWith "sibene"   t -> conv          (7  + acc) t
        | StartsWith "achte"    t -> conv          (8  + acc) t
        | StartsWith "nüne"     t -> conv          (9  + acc) t

        | StartsWith "zwänzg"   t -> conv         (20  + acc) t
        | StartsWith "drissg"   t -> conv         (30  + acc) t
        | StartsWith "vierzg"   t -> conv         (40  + acc) t
        | StartsWith "füfzg"    t -> conv         (50  + acc) t
        | StartsWith "sächzg"   t -> conv         (60  + acc) t
        | StartsWith "sibezg"   t -> conv         (70  + acc) t
        | StartsWith "achtzg"   t -> conv         (80  + acc) t
        | StartsWith "nachtzg"  t -> conv         (80  + acc) t
        | StartsWith "nünzg"    t -> conv         (90  + acc) t

        | StartsWith "null"     t -> conv          (0  + acc) t
        | StartsWith "eis"      t -> conv          (1  + acc) t
        | StartsWith "zwöi"     t -> conv          (2  + acc) t
        | StartsWith "drü"      t -> conv          (3  + acc) t
        | StartsWith "vier"     t -> conv          (4  + acc) t
        | StartsWith "föif"     t -> conv          (5  + acc) t
        | StartsWith "sächs"    t -> conv          (6  + acc) t
        | StartsWith "sibe"     t -> conv          (7  + acc) t
        | StartsWith "acht"     t -> conv          (8  + acc) t
        | StartsWith "nün"      t -> conv          (9  + acc) t

        | StartsWith "hundert"  t ->
            conv (if acc = 0 then  100 else       100 %* acc) t
        | StartsWith "tusig" t ->
            conv (if acc = 0 then 1000 else      1000 %* acc) t
        | StartsWith " millione " t
        | StartsWith " millione" t
        | StartsWith " million " t
        | StartsWith " million"  t ->
            conv (if acc = 0 then 1000000 else 1000000 %* acc) t
        | StartsWith " milliarde "  t
        | StartsWith " milliarde"  t ->
            conv (if acc = 0 then 1000000000 else 1000000000 %* acc) t

        | StartsWith "ei"       t
        | StartsWith "und"      t -> conv                acc  t

        | _                       -> None

    let canonicalized = x.Trim().ToLower(System.Globalization.CultureInfo "en")
    match canonicalized with
    | EndsWith " unger null" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized