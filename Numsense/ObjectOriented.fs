﻿namespace Ploeh.Numsense.ObjectOriented

open System.Runtime.InteropServices
open Ploeh.Numsense

type INumeralConverter =
    abstract ToNumeral : number : int -> string
    abstract TryParse : s : string * [<Out>]result : int byref -> bool

module internal Helper =
    let tryParse f (s, result : int byref) =
        match isNull s, lazy (f s) with
        | true, _
        | false, Lazy None -> false
        | false, Lazy (Some i) -> result <- i; true

type EnglishNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toEnglish number
        member this.TryParse (s, result) = 
            Helper.tryParse Numeral.tryParseEnglish (s, &result)

type DanishNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toDanish number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParseDanish (s, &result)

type PolishNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toPolish number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParsePolish (s, &result)

type DutchNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toDutch number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParseDutch (s, &result)

type RussianNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toRussian number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParseRussian (s, &result)
            
type CatalanNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toCatalan number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParseCatalan (s, &result)

type FarsiNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toFarsi number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParseFarsi (s, &result)

type BerneseNumeralConverter () =
    interface INumeralConverter with
        member this.ToNumeral number = Numeral.toBernese number
        member this.TryParse (s, result) =
            Helper.tryParse Numeral.tryParseBernese (s, &result)
       
type Numeral private () =
    static member val English = EnglishNumeralConverter () :> INumeralConverter
    static member val Farsi   = FarsiNumeralConverter () :> INumeralConverter
    static member val Danish  = DanishNumeralConverter () :> INumeralConverter
    static member val Polish  = PolishNumeralConverter () :> INumeralConverter
    static member val Dutch   = DutchNumeralConverter () :> INumeralConverter
    static member val Russian = RussianNumeralConverter () :> INumeralConverter
    static member val Catalan = CatalanNumeralConverter () :> INumeralConverter
    static member val Bernese = BerneseNumeralConverter () :> INumeralConverter
