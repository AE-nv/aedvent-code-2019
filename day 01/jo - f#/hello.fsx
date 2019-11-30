#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

test <@ 1 + 2 * 3 = 8 @>