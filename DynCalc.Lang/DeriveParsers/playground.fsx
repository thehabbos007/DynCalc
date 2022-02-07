#r "nuget: FSharp.Data"
#load "./JsonPath.fs"

open System
open System.Net
open FSharp.Data

JsonValue.Parse
    """
                {"a":{
                  "b":{
                   "a":{
                    "a":{
                     "b":{
                      "c":2
                }}}}}}
            """
|> JsonPath.tryFind "$..c"

JsonValue.Parse
    """
{"data":[{"p":7296.89,"s":"BINANCE:BTCUSDT","t":1575526691134,"v":0.011467},{"p":7236.89,"s":"BINANCE:BTCUSDT","t":1575526691134,"v":0.011467}],"type":"trade"}"""
|> JsonPath.tryFind "$.data[-1].p"


let value =
    JsonValue.Parse
        """
{"data":[{"p":7296.89,"s":"BINANCE:BTCUSDT","t":1575526691134,"v":0.011467},{"p":7236.89,"s":"BINANCE:BTCUSDT","t":1575526691134,"v":0.011467}],"type":"trade"}"""

JsonPath.findList "$.data[0].p" value

async { return! Http.AsyncRequestString("http://www.floatrates.com/daily/dkk.json") }
|> Async.RunSynchronously
|> JsonValue.Parse
|> JsonPath.findList "$.usd.inverseRate"



// A1 :: int (deriving (json "data.price"))
