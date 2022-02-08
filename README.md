# DynCalc
B.Eng. project 2021.

**Example DynCalc program*(:
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

`avg` function is equivalent to division with the sum of list elements in the numerator and length of elements in denominator. Input is a list of numbers.

`avg_in_dkk` composes avg with multiplication by a constant. The input is a list `<usd_to_dkk, btc_price>`

Example data that could be supplied to `avg_in_dkk`: `<6.6, <44045.20, 44000, 44050.02>>`
Which is equivalent to the following mathematical expression: `6.6 * ((44045.20 + 44000 + 44050.02) / 3)`
