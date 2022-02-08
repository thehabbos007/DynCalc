# DynCalc
B.Eng. project 2021.

**Example DynCalc program**:
```
type StockPrice :: Number[] deriving (json "$.data[*].p");
btc_price <- ticker("BINANCE:BTCUSDT") as StockPrice;

type ConversionRate :: Number deriving (json "$.usd.inverseRate");
usd_to_dkk <- http("http://www.floatrates.com/daily/dkk.json", "5000") as ConversionRate;

func avg = / . [%+, length];
func avg_in_dkk = * . [id . 0, avg . 1];

btc_avg_price_in_dkk = avg_in_dkk:<usd_to_dkk, btc_price>;
btc_avg_price_in_dkk -> http();
btc_avg_price_in_dkk -> log();
```
The `ticker` provider uses the Finnhub.io WS API to fetch live stock data. The data received is put through the `$.data[*].p"` JSONPath expression resulting in a list of numbers.

The `http` provider polls the URL given as the first parameter and an interval to poll in, 5 seconds here.

`avg` function is equivalent to division with the sum of list elements in the numerator and length of elements in denominator. Input is a list of numbers.

`avg_in_dkk` composes avg with multiplication by a constant. The input is a list `<usd_to_dkk, btc_price>`

Example data that could be supplied to `avg_in_dkk`: `<6.6, <44045.20, 44000, 44050.02>>`
Which is equivalent to the following mathematical expression: `6.6 * ((44045.20 + 44000 + 44050.02) / 3)`

The `http` sinks exposes data on the left side to plain text on a HTTP endpoint with the unique ID of the program instance. `/api/data/<id>`

## Running the system
In order to run the DynCalc runtime system, the following requirements must be met on the host machine. 
The versions might be minimum versions, but these are the versions that have been used while developing:
- .NET SDK 5.0.402
- Microsoft.AspNetCore.App runtime 5.0.11
- Microsoft.NETCore.App runtime 5.0.11

Running the system can be done by downloading the source code, unzipping and executing the following command
```sh
dotnet run -p DynCalc.Web
```
at the root of the DynCalc directory.

To run test code the following command can be executed
```
dotnet test
```
