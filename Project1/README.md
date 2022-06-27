ST 558 Project 1
================
John Hinic

# Querying APIs in R

## Introduction

The purpose of this vignette is to give a brief overview of accessing
APIs within R, writing custom functions that make the process easier,
and performing some basic exploratory data analysis (EDA) on the data we
use.

The API we are going to utilize is
[Polygon.io](https://polygon.io/docs/getting-started), which contains
some financial data of various stocks, options, and currencies.

### Required Packages

The following packages (and their purpose) will be required:

-   `tidyverse`: for general data manipulation and plotting
-   `httr`: used to query the API source
-   `jsonlite`: used to parse the JSON data returned from the API

### Basic API Info

The Polygon.io API contains a plethora of financial data pertaining to
the stock markets as well as cryptocurrency. As far as pulling actual
market data, there are several possible endpoints in the free version of
the API:

-   Aggregates (Bars)
-   Grouped Daily (Bars)
-   Daily Open/Close
-   Previous Close

There are also many endpoints for reference data, which give more
details to inform queries. These reference endpoints contain things like
the tickers themselves, the types of tickers, news about tickers, etc.

It is not feasible to explore the entire API in this vignette, so we
will focus on the “Aggregates (Bars)” market data endpoint, as well as
the “Ticker Details V3” reference data endpoint.

## Accessing the API

Accessing desired parts of the API essentially boils down to
constructing the correct URL. To do this, we start with the base URL
(`https://api.polygon.io`), then add onto that based on which data we
want. As stated, we are going to focus on exploring one market data
endpoint and one reference data endpoint. Each of these endpoints has a
specific URL format that you will need to add to the end of the base
URL, with several parameters for the user to specify within the “{}”.
The formats are as follows:

-   Aggregates (Bars):
    `/v2/aggs/ticker/{stocksTicker}/range/{multiplier}/{timespan}/{from}/{to}`
    -   stocksTicker: the ticker symbol for the stock data we want to
        pull
    -   multiplier: what to multiply the specified timespan by (i.e. if
        multiplier = 3 and timespan = day, the 3-day bars will be
        returned)
    -   timespan: the size of the time window for the bars (can be
        minute, hour, day, week, month, quarter, or year)
    -   from: the start date in format “YYYY-MM-DD” (up to 2 years prior
        to the current date)
    -   to: the end date in format “YYYY-MM-DD”
-   Ticker Details V3: `/v3/reference/tickers/{ticker}`
    -   ticker: the ticker symbol we want to pull the details of

There are also some options that are not required that the user can
specify, such as whether the results should be adjusted for splits
(`TRUE` by default), whether the sort should be ascending or descending,
and a limit for the number of results. To specify these options, you
would construct the URL as previously described, put a ‘?’ at the end,
and then specify the desired options (separated by ‘&’).

For any query of the API, you will also need to supply your API key at
the end (similar to other options, you would specify
‘apiKey=*yourkey*’).

We are going to write some custom functions that allow the user to
specify which data they want, and will then construct the URL, parse the
data, and return a neat dataframe for the user to analyze.

### Writing Functions

The first function will be designed to query the bars endpoint. It will
require several arguments from the user:

-   ticker (can be the symbol for any company or the full company name
    for a select few)
-   start/end dates (must be in YYYY-MM-DD format)
-   multiplier (as described above)
-   timespan (as described above - minute, hour, day, week, month,
    quarter, or year)
-   apikey (the user’s API key)

As stated, the ticker can be the official ticker symbol (i.e. “AAPL” for
Apple) for any company, but will accept the following company names as
well:

-   “Apple”, “Apple Inc.”
-   “Google”, “Alphabet Inc. Class A”
-   “Microsoft”, “Microsoft Corporation”
-   “Amazon”, “Amazon.com, Inc.”

Aside from the required arguments, there will also be several options
that the user can specify if desired:

-   adjusted (boolean, whether the results should be adjusted for
    splits)
-   sort (“asc”/“ascending” or “desc”/“descending”)
-   limit (the maximum number of base aggregates used to create
    aggregate results)

The returned data frame will only contain the relevant columns, not the
data pertaining to the query itself.

``` r
pullBars <- function(
    ticker, 
    start, 
    end, 
    multiplier = 1, 
    timespan = "day", 
    adj = TRUE, 
    sort = "asc", 
    limit = 50000,
    apikey = "meDW5a0CdiQrg_pqfIRI1yanOQ3FwPFu"
) {
  # resolving ticker symbols
  tick <- switch(
    tolower(ticker),
    "apple" = ,
    "apple inc." = "AAPL",
    "google" = ,
    "alphabet class a inc." = "GOOGL",
    "amazon" = ,
    "amazon.com, inc." = "AMZN",
    "microsoft" = ,
    "microsoft corporation" = "MSFT",
    ticker
  )
  base <- "https://api.polygon.io/v2/aggs/ticker/"
  #/v2/aggs/ticker/{stocksTicker}/range/{multiplier}/{timespan}/{from}/{to}
  base2 <- paste0(base, tick, '/range/', as.character(multiplier), '/', timespan, '/', start, '/', end, '/')
  
  # resolving sort values
  srt <- switch(
    tolower(sort),
    "asc" = ,
    "ascending" = "sort=asc",
    "desc" = ,
    "descending" = "sort=desc"
  )
  
  # resolving adjusted option
  adjOpt <- switch(
    as.character(adj),
    "TRUE" = "adjusted=true",
    "FALSE" = "adjusted=false"
  )
  
  # resolving limit option
  lim <- paste0("limit=", as.character(limit))
  
  # resolving key
  key <- paste0("apiKey=", apikey)
  
  # combining options into single string
  opts <- paste(srt, adjOpt, lim, key, sep = "&")
  
  # combining base URL with options for final URL
  url <- paste(base2, opts, sep = "?")
  
  # querying data and converting to data frame with only relevant columns
  data <- GET(url) %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    as.data.frame()
  data %>% colnames() %>% print()

  final <- data %>%  
    select(starts_with("ticker"), volume = results.v, volWeighted = results.vw, open = results.o,
           close = results.c, high = results.h, low = results.l, transactions = results.n)
  
  return(final)
}
```

With the function to pull the actual market data complete, we will now
construct a function that pulls reference data with specific details
about the supplied ticker and overall company. This function will be
similar to the previous, but have fewer arguments due to the lower level
of complexity:

-   ticker (identical to previous function)
-   date (optional, will return information about the supplied ticker on
    the specific date. Must be in YYYY-MM-DD format)
-   apikey (the user’s API key)

This function will return

``` r
pullDetails <- function(
    ticker,
    date = '',
    apikey = 'meDW5a0CdiQrg_pqfIRI1yanOQ3FwPFu'
) {
  # resolving ticker symbols
  tick <- switch(
    tolower(ticker),
    "apple" = ,
    "apple inc." = "AAPL",
    "google" = ,
    "alphabet class a inc." = "GOOGL",
    "amazon" = ,
    "amazon.com, inc." = "AMZN",
    "microsoft" = ,
    "microsoft corporation" = "MSFT",
    ticker
  )
  
  # resolving key
  key <- paste0("apiKey=", apikey)
  
  # resolving date if specified
  if(length(date) > 0){
    dateOpt <- paste0('date=', date, '&')
  } else {
    dateOpt <- ''
  }
  
  # constructing final URL
  base <- 'https://api.polygon.io/v3/reference/tickers/'
  opts <- paste0(tick, '?', dateOpt, key)
  url <- paste0(base, opts)
  
  # querying data and converting to data frame
  GET(url) %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    as.data.frame()
}
```

### Function Demonstration

Now that both functions are completed, we will demonstrate some basic
usage of them. For instance, we can pull daily aggregate (adjusted) data
about Google from the beginning of 2022 until the start of June:

``` r
pullBars("Google", "2022-01-01", "2022-06-01")
```

    ##  [1] "ticker"       "queryCount"   "resultsCount" "adjusted"     "results.v"    "results.vw"   "results.o"   
    ##  [8] "results.c"    "results.h"    "results.l"    "results.t"    "results.n"    "status"       "request_id"  
    ## [15] "count"

    ##     ticker  volume volWeighted     open   close     high      low transactions
    ## 1    GOOGL 1433947    2899.406 2901.100 2899.83 2917.020 2874.255       103431
    ## 2    GOOGL 1419962    2894.441 2907.920 2887.99 2929.698 2874.325       102667
    ## 3    GOOGL 2730914    2800.833 2888.400 2755.50 2889.988 2753.760       199566
    ## 4    GOOGL 1867371    2767.210 2739.970 2754.95 2798.800 2731.170       137860
    ## 5    GOOGL 1488028    2741.796 2762.910 2740.34 2768.970 2715.330       111612
    ## 6    GOOGL 2220406    2723.539 2701.560 2773.39 2776.390 2663.290       184011
    ## 7    GOOGL 1436485    2781.827 2760.140 2794.72 2804.320 2733.845       100618
    ## 8    GOOGL 1305602    2831.515 2823.000 2828.61 2852.155 2813.890        92377
    ## 9    GOOGL 1571830    2803.068 2830.800 2771.74 2857.000 2768.180       110059
    ## 10   GOOGL 1483064    2788.653 2741.580 2789.61 2814.840 2739.970       101814
    ## 11   GOOGL 1744405    2723.509 2723.500 2719.96 2742.630 2708.765       127973
    ## 12   GOOGL 1432382    2731.250 2730.470 2702.33 2759.190 2700.310        95860
    ## 13   GOOGL 1495357    2711.310 2725.000 2666.15 2752.520 2659.293       113285
    ## 14   GOOGL 2781021    2647.849 2651.870 2607.03 2697.311 2601.730       179951
    ## 15   GOOGL 3834712    2549.978 2519.560 2616.08 2624.060 2490.000       261299
    ## 16   GOOGL 2348028    2553.207 2574.800 2538.70 2587.980 2530.010       144722
    ## 17   GOOGL 2451475    2603.152 2622.380 2584.66 2660.000 2542.840       157806
    ## 18   GOOGL 1600939    2610.531 2626.080 2580.10 2653.045 2578.646       124746
    ## 19   GOOGL 1718074    2629.422 2593.180 2667.02 2667.130 2569.700       123757
    ## 20   GOOGL 1999300    2685.677 2683.240 2706.07 2709.460 2644.190       145681
    ## 21   GOOGL 3426282    2794.036 2751.890 2752.88 2755.855 2687.370       263412
    ## 22   GOOGL 6159961    2970.289 3025.000 2960.00 3030.932 2910.450       472215
    ## 23   GOOGL 3575421    2919.346 2914.110 2861.80 2993.500 2853.306       285469
    ## 24   GOOGL 2899911    2860.782 2868.893 2865.86 2897.500 2810.300       215947
    ## 25   GOOGL 2748665    2815.259 2885.000 2784.02 2885.000 2778.000       206650
    ## 26   GOOGL 2714180    2774.160 2780.530 2787.98 2800.000 2740.277       182224
    ## 27   GOOGL 2007194    2824.948 2819.270 2831.84 2850.485 2803.741       148118
    ## 28   GOOGL 1966533    2787.698 2794.070 2772.40 2829.690 2759.140       171567
    ## 29   GOOGL 1992917    2715.662 2772.000 2685.65 2783.125 2668.000       199155
    ## 30   GOOGL 1715054    2702.717 2665.130 2710.52 2726.000 2665.130       130001
    ## 31   GOOGL 1335563    2734.193 2751.410 2732.17 2762.170 2716.430       100139
    ## 32   GOOGL 1251065    2732.586 2732.930 2754.76 2761.720 2698.475       103758
    ## 33   GOOGL 1947475    2681.900 2724.860 2650.78 2742.270 2649.570       161496
    ## 34   GOOGL 2179872    2630.627 2669.340 2608.06 2681.400 2603.930       158081
    ## 35   GOOGL 2242037    2600.946 2596.762 2596.27 2643.605 2564.465       170077
    ## 36   GOOGL 1639301    2580.849 2632.970 2551.76 2641.020 2550.550       145602
    ## 37   GOOGL 2719619    2584.156 2499.065 2653.82 2660.000 2499.065       227050
    ## 38   GOOGL 1820299    2677.142 2671.040 2689.19 2705.430 2635.030       136500
    ## 39   GOOGL 1943221    2686.384 2661.000 2701.14 2715.030 2651.845       124537
    ## 40   GOOGL 1317268    2689.852 2697.570 2681.23 2725.110 2664.370       110974
    ## 41   GOOGL 1168490    2689.578 2692.560 2691.43 2705.250 2661.431       103434
    ## 42   GOOGL 1230162    2690.150 2720.200 2677.99 2728.790 2660.510       107530
    ## 43   GOOGL 1522532    2631.432 2658.550 2638.13 2677.030 2606.750       131108
    ## 44   GOOGL 2255647    2555.822 2629.760 2527.57 2634.700 2525.900       213009
    ## 45   GOOGL 2021355    2558.680 2525.000 2542.09 2625.000 2513.494       158099
    ## 46   GOOGL 1851270    2643.669 2625.000 2668.40 2674.710 2598.010       132417
    ## 47   GOOGL 1355070    2640.138 2625.320 2648.59 2665.435 2618.250       113022
    ## 48   GOOGL 1576322    2626.392 2669.770 2597.41 2675.910 2592.630       116520
    ## 49   GOOGL 1940563    2539.643 2605.000 2519.02 2608.180 2505.500       169621
    ## 50   GOOGL 1545976    2566.955 2539.520 2583.96 2597.090 2514.410       122172
    ## 51   GOOGL 1778849    2637.309 2614.660 2665.61 2666.660 2574.690       131832
    ## 52   GOOGL 1413196    2667.193 2662.980 2676.78 2681.608 2643.700       104280
    ## 53   GOOGL 2223345    2696.686 2668.490 2722.51 2724.880 2645.165       128817
    ## 54   GOOGL 1341584    2714.662 2723.270 2722.03 2741.000 2681.845       110283
    ## 55   GOOGL 1774777    2792.127 2722.030 2797.36 2821.000 2722.030       126456
    ## 56   GOOGL 1212670    2775.194 2774.050 2765.51 2791.770 2756.700       100972
    ## 57   GOOGL 1314828    2806.827 2784.000 2831.44 2832.375 2755.010        92646
    ## 58   GOOGL 1206154    2825.559 2838.320 2833.46 2840.700 2794.750        93037
    ## 59   GOOGL 1742463    2823.787 2818.010 2829.11 2840.050 2796.225       112885
    ## 60   GOOGL 1715851    2856.825 2852.950 2850.11 2875.870 2840.765       123693
    ## 61   GOOGL  994238    2842.137 2849.200 2838.77 2854.406 2832.000        83578
    ## 62   GOOGL 1874961    2800.040 2841.060 2781.35 2842.155 2780.490       112935
    ## 63   GOOGL 1297072    2794.688 2790.000 2803.01 2809.415 2766.150        90813
    ## 64   GOOGL 1298542    2855.003 2807.170 2859.43 2874.240 2806.210        97491
    ## 65   GOOGL 1070906    2823.046 2857.380 2811.82 2859.810 2807.650        95221
    ## 66   GOOGL 1623424    2739.196 2775.000 2730.96 2787.210 2710.340       144792
    ## 67   GOOGL 1311680    2717.970 2720.200 2717.77 2743.290 2684.548       111352
    ## 68   GOOGL 1257140    2682.823 2711.670 2665.75 2713.400 2659.310       106431
    ## 69   GOOGL 1842753    2596.368 2636.470 2576.47 2641.850 2573.370       170455
    ## 70   GOOGL 1635535    2573.906 2632.000 2554.29 2632.000 2535.780       152976
    ## 71   GOOGL 1396362    2585.445 2560.000 2597.88 2606.680 2554.825       107359
    ## 72   GOOGL 1579899    2553.418 2609.590 2534.60 2609.590 2532.020       138659
    ## 73   GOOGL 1040433    2547.162 2540.000 2553.53 2564.250 2523.640       100576
    ## 74   GOOGL 1324488    2586.475 2553.830 2600.18 2606.660 2539.990       102794
    ## 75   GOOGL 1577273    2576.199 2617.840 2560.80 2627.980 2550.000       131898
    ## 76   GOOGL 1822258    2525.101 2585.000 2496.29 2601.970 2490.530       188011
    ## 77   GOOGL 2842684    2420.596 2500.000 2392.71 2508.040 2378.500       274346
    ## 78   GOOGL 2304693    2432.846 2382.020 2461.48 2465.550 2370.220       173772
    ## 79   GOOGL 3643754    2372.912 2445.890 2373.00 2450.200 2370.220       293376
    ## 80   GOOGL 4591029    2292.823 2289.385 2285.89 2344.840 2254.730       304175
    ## 81   GOOGL 2444232    2339.114 2328.300 2370.45 2392.990 2282.300       162079
    ## 82   GOOGL 2133800    2309.742 2334.690 2282.19 2358.395 2276.290       156738
    ## 83   GOOGL 1776651    2301.972 2268.100 2331.66 2334.910 2252.000       139070
    ## 84   GOOGL 1248368    2345.512 2328.610 2346.68 2368.836 2320.690        96303
    ## 85   GOOGL 2495764    2385.657 2340.630 2445.22 2457.090 2302.320       142549
    ## 86   GOOGL 2291385    2334.426 2404.080 2330.11 2420.780 2300.115       177901
    ## 87   GOOGL 1980479    2316.623 2303.690 2314.93 2351.430 2280.310       141071
    ## 88   GOOGL 2040143    2264.735 2265.000 2250.22 2301.542 2240.010       165709
    ## 89   GOOGL 1995019    2295.481 2310.150 2287.90 2325.000 2258.010       137067
    ## 90   GOOGL 1876737    2286.383 2264.730 2272.05 2327.288 2264.730       128065
    ## 91   GOOGL 2691809    2240.816 2227.550 2256.88 2285.900 2196.490       184669
    ## 92   GOOGL 1751924    2319.571 2290.660 2321.01 2357.500 2272.100       121920
    ## 93   GOOGL 1299522    2293.789 2299.110 2288.90 2323.400 2277.790        97074
    ## 94   GOOGL 1152658    2321.028 2336.810 2329.46 2338.000 2297.395        85183
    ## 95   GOOGL 1756299    2258.429 2300.000 2237.99 2308.000 2231.110       132775
    ## 96   GOOGL 1700120    2224.656 2228.630 2207.68 2260.200 2200.000       123169
    ## 97   GOOGL 2448148    2163.565 2239.020 2178.16 2243.670 2115.927       165230
    ## 98   GOOGL 1859247    2214.092 2191.750 2229.76 2242.950 2174.820       121005
    ## 99   GOOGL 3838989    2090.080 2115.440 2119.40 2129.165 2037.694       252378
    ## 100  GOOGL 2012867    2107.895 2099.710 2116.10 2130.000 2077.130       126949
    ## 101  GOOGL 1897361    2152.183 2113.540 2155.85 2172.990 2104.497       109716
    ## 102  GOOGL 1895706    2230.240 2189.660 2246.33 2246.360 2182.765       115567
    ## 103  GOOGL 2490613    2278.789 2254.930 2275.24 2314.700 2241.570       136457
    ## 104  GOOGL 1830276    2294.603 2297.100 2277.84 2342.031 2265.000       120997

Notice that despite the many possible arguments of the function, we can
simply specify the ticker, start date, and end date and it will return
the data with reasonable defaults. We can also pull the company details
using the second function (this time using the actual ticker symbol):

``` r
pullDetails("GOOGL")
```

    ##   results.ticker                       results.name results.market results.locale results.primary_exchange
    ## 1          GOOGL Alphabet Inc. Class A Common Stock         stocks             us                     XNAS
    ##   results.type results.active results.currency_name results.cik results.composite_figi results.share_class_figi
    ## 1           CS           TRUE                   usd  0001652044           BBG009S39JX6             BBG009S39JY5
    ##   results.market_cap results.phone_number  results.address.address1 results.address.city results.address.state
    ## 1        1.55373e+12         650-253-0000 1600 AMPHITHEATRE PARKWAY        MOUNTAIN VIEW                    CA
    ##   results.address.postal_code
    ## 1                       94043
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          results.description
    ## 1 Alphabet Inc is a holding company, with Google, the Internet media giant, as a wholly owned subsidiary. Google generates 99% of Alphabet revenue, of which more than 85% is from online ads. Google's other revenue is from sales of apps and content on Google Play and YouTube, as well as cloud service fees and other licensing revenue. Sales of hardware such as Chromebooks, the Pixel smartphone, and smart homes products, which include Nest and Google Home, also contribute to other revenue. Alphabet's moonshot investments are in its other bets segment, where it bets on technology to enhance health (Verily), faster Internet access to homes (Google Fiber), self-driving cars (Waymo), and more. Alphabet's operating margin has been 25%-30%, with Google at 30% and other bets operating at a loss.
    ##   results.sic_code                              results.sic_description results.ticker_root results.ticker_suffix
    ## 1             7370 SERVICES-COMPUTER PROGRAMMING, DATA PROCESSING, ETC.                GOOG                     L
    ##   results.homepage_url results.total_employees results.list_date results.share_class_shares_outstanding
    ## 1  https://www.abc.xyz                  163906        2004-08-19                              300760000
    ##   results.weighted_shares_outstanding status                       request_id
    ## 1                           658499877     OK e201a90c332880c68820dad85eaa5735

We could also change some options with the market data we pull. For
example, we can pull weekly aggregate (adjusted) data about Microsoft
from all of 2021:

``` r
msft <- pullBars("Microsoft", "2021-01-01", "2021-12-31", timespan = "week")
```

    ##  [1] "ticker"       "queryCount"   "resultsCount" "adjusted"     "results.v"    "results.vw"   "results.o"   
    ##  [8] "results.c"    "results.h"    "results.l"    "results.t"    "results.n"    "status"       "request_id"  
    ## [15] "count"

``` r
msft
```

    ##    ticker    volume volWeighted     open  close     high      low transactions
    ## 1    MSFT  76300478    223.3648 224.4500 222.42 227.1800 219.6800       791051
    ## 2    MSFT 147520111    216.9389 222.5300 219.62 223.0000 211.9400      1594339
    ## 3    MSFT 126960294    215.0147 218.4700 212.65 218.9100 212.0300      1275648
    ## 4    MSFT 129177135    222.9220 213.7500 225.95 230.0700 212.6300      1283967
    ## 5    MSFT 243723031    234.6231 229.1200 231.96 242.6400 224.2200      2758975
    ## 6    MSFT 129514486    240.8368 235.0600 242.20 245.0900 232.4300      1478121
    ## 7    MSFT 100265726    243.1295 243.1500 244.99 245.9200 240.8100      1056654
    ## 8    MSFT  89452259    242.6885 245.0300 240.97 246.1300 240.1800       881153
    ## 9    MSFT 170135168    232.5341 237.4200 232.38 237.9300 227.8800      2077270
    ## 10   MSFT 168682727    230.8671 235.9000 231.60 237.4700 224.2600      2448378
    ## 11   MSFT 150625031    233.7987 231.3700 235.75 239.1700 227.1300      2101379
    ## 12   MSFT 164911946    233.6912 234.9600 230.35 240.0550 229.3496      1943563
    ## 13   MSFT 146912864    235.6140 230.2700 236.48 241.0500 230.1400      1846070
    ## 14   MSFT 123972320    236.2029 236.5900 242.35 242.8400 231.1000      1404537
    ## 15   MSFT 130373147    250.5659 242.7600 255.85 255.9900 242.7000      1506538
    ## 16   MSFT 124563138    257.9493 254.7100 260.74 261.0000 254.6200      1543702
    ## 17   MSFT 114015581    259.1389 260.1900 261.15 261.7800 255.6400      1464692
    ## 18   MSFT 168964799    255.1618 261.6600 252.18 263.1900 249.0000      2223055
    ## 19   MSFT 127581742    249.4466 253.4000 252.46 254.3500 244.6900      1661264
    ## 20   MSFT 153112228    244.7101 250.8700 248.15 251.7300 238.0700      1776359
    ## 21   MSFT 114508724    244.6140 246.5500 245.17 248.3300 238.6000      1329014
    ## 22   MSFT  99575336    250.6823 247.7850 249.68 252.9400 247.5100      1060908
    ## 23   MSFT  93009220    247.8853 251.2300 250.79 251.6500 243.0000       999043
    ## 24   MSFT 106985149    254.8513 249.9800 257.89 258.4868 249.8100      1174494
    ## 25   MSFT 129115697    259.2211 257.9000 259.43 262.3000 254.4200      1338431
    ## 26   MSFT 116146870    264.7156 259.8200 265.02 267.8500 257.9200      1239720
    ## 27   MSFT 104258323    271.6284 266.1850 277.65 278.0000 265.9100      1190990
    ## 28   MSFT 103108193    277.7930 278.0300 277.94 280.6945 274.3000      1262423
    ## 29   MSFT 116956560    280.9168 279.1570 280.75 284.1000 276.5800      1365818
    ## 30   MSFT 129711125    281.7096 278.9335 289.67 289.9900 274.4500      1461734
    ## 31   MSFT 129365745    286.5566 289.0000 284.91 290.1500 282.9500      1565654
    ## 32   MSFT  80817639    286.9369 286.3600 289.46 289.6300 283.7400      1008711
    ## 33   MSFT  81509578    288.6485 289.7500 292.85 292.9000 285.2000       957547
    ## 34   MSFT 134876629    296.4695 293.1900 304.36 305.8400 288.6400      1550926
    ## 35   MSFT 101151298    301.7880 303.2450 299.72 305.6500 296.8300      1226590
    ## 36   MSFT  92578490    302.4566 301.1150 301.14 305.1900 300.1800      1098314
    ## 37   MSFT  71976514    298.7091 301.0050 295.71 302.1400 295.3800      1087800
    ## 38   MSFT 135772647    300.9622 297.5500 299.87 305.3200 294.0800      1523421
    ## 39   MSFT 120872685    296.6018 296.3300 299.35 300.9000 289.5200      1635485
    ## 40   MSFT 156105387    286.5897 296.1400 289.10 296.4700 281.2900      2070438
    ## 41   MSFT 122710419    289.5200 287.4000 294.85 296.6409 280.2500      1694436
    ## 42   MSFT 112588007    298.2066 292.9200 304.21 304.4500 292.3500      1391688
    ## 43   MSFT  91240936    307.9591 303.5700 309.16 311.0900 302.6900      1220030
    ## 44   MSFT 159313433    320.4513 309.3600 331.62 332.0000 306.4600      1935249
    ## 45   MSFT 121242671    332.9083 331.3550 336.06 338.7900 326.3700      1643556
    ## 46   MSFT 108186069    334.4877 337.3000 336.72 338.7200 329.9200      1372271
    ## 47   MSFT 100668957    339.8152 337.5400 343.11 345.1000 334.0340      1269929
    ## 48   MSFT 107156474    337.7542 344.6200 329.68 349.6700 328.1200      1422276
    ## 49   MSFT 177334367    330.6815 334.9400 323.01 339.2800 318.0300      2380156
    ## 50   MSFT 146125331    333.5749 323.9500 342.54 343.0000 319.2300      1929092
    ## 51   MSFT 191637792    328.9164 340.6800 323.80 343.7900 317.2500      2344582
    ## 52   MSFT  97516470    326.8824 320.0500 334.69 336.3900 317.5700      1364356
    ## 53   MSFT  84640440    340.3078 335.4600 336.32 344.3000 335.4300      1196308

### Analysis Data

Now that we have written and tested our functions, we are going to use
them to pull some data for basic exploratory data analysis. We are going
to pull the weekly aggregate (adjusted) data of Apple, Microsoft,
Google, and Amazon going from all of 2021, then concatenate them all
into a single dataset with an added variable called “week” that is
simply an indicator for what number week it is in the year.

``` r
apple <- pullBars("Apple", "2021-01-01", "2021-12-31", timespan = "week")
```

    ##  [1] "ticker"       "queryCount"   "resultsCount" "adjusted"     "results.v"    "results.vw"   "results.o"   
    ##  [8] "results.c"    "results.h"    "results.l"    "results.t"    "results.n"    "status"       "request_id"  
    ## [15] "count"

``` r
google <- pullBars("Google", "2021-01-01", "2021-12-31", timespan = "week")
```

    ##  [1] "ticker"       "queryCount"   "resultsCount" "adjusted"     "results.v"    "results.vw"   "results.o"   
    ##  [8] "results.c"    "results.h"    "results.l"    "results.t"    "results.n"    "status"       "request_id"  
    ## [15] "count"

``` r
amazon <- pullBars("AMZN", "2021-01-01", "2021-12-31", timespan = "week")
```

    ##  [1] "ticker"       "queryCount"   "resultsCount" "adjusted"     "results.v"    "results.vw"   "results.o"   
    ##  [8] "results.c"    "results.h"    "results.l"    "results.t"    "results.n"    "status"       "request_id"  
    ## [15] "count"

``` r
all <- bind_rows(apple, google, msft, amazon) %>%
  mutate(week = rep(seq(1:53), 4))
```

We are also going to derive another variable of interest. We are going
to subtract the open price from the closing price, then take that as a
proportion of the weighted average (and multiply that number by 100 to
make the numbers more readable). This will give us a measure of growth
that is proportional to the overall price of the stock for each company.

``` r
final <- all %>%
  mutate(growth = 100*(close - open) / volWeighted)
```

## Exploratory Data Analysis

For our exploratory data analysis, we are primarily going to look at the
previously derived `growth` variable as our response. We are going to
look at this across the different companies, as well as weeks of the
year.

### Univariate Summaries

To start, we will look at some simple summaries of our growth variable.

``` r
summary(final$growth)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -9.8734 -1.4395  0.4052  0.5388  2.3506 12.1432

We can see that our mean is slightly higher that our median, which means
the data might be a little right skewed. The fact that both measures of
center are above 0 indicates that, on average, all 4 companies had
positive growth over 2021. To further investigate, we will look at a
histogram:

``` r
final %>% ggplot(aes(growth)) +
  geom_histogram(bins = 20) +
  labs(title = "Histogram of Growth Rate", x = "Growth Rate") +
  theme_minimal()
```

![](README_files/figure-gfm/growth%20hist-1.png)<!-- -->

It appears that the data is very slightly right skewed, but is pretty
symmetrical for the most part.

### Bivariate Summaries

To start with, we will look at summaries of the growth rate across the 4
companies.

``` r
final %>%
  group_by(ticker) %>%
  summarise(Median = median(growth),
            Mean = mean(growth),
            StD = sd(growth),
            Min = min(growth),
            Max = max(growth))
```

    ## # A tibble: 4 x 6
    ##   ticker Median   Mean   StD   Min   Max
    ##   <chr>   <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 AAPL    0.271 0.446   3.31 -7.97  8.78
    ## 2 AMZN   -0.126 0.0882  3.36 -9.87  6.07
    ## 3 GOOGL   0.549 0.909   3.19 -4.92 12.1 
    ## 4 MSFT    0.756 0.712   2.66 -5.13  6.95

Here, it looks like 3 of the 4 companies had a positive growth rate, but
Amazon actually had negative median growth rate throughout 2021.
However, the mean is still above 0 - this would mean that Amazon had
more negative growth weeks than positive, but overall the positive
growth weeks slightly outweighed the negative.

We can also look at the histograms grouped by company (with a reference
line at 0):

``` r
final %>% ggplot(aes(growth, fill = ticker)) +
  geom_histogram(bins = 20) +
  labs(title = "Histograms of Growth Rate by Company", x = "Growth Rate") +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  facet_wrap(~ticker)
```

![](README_files/figure-gfm/histogram%20by%20company-1.png)<!-- -->

Here, it looks like Microsoft and Google clearly had better years than
Apple and Amazon. It appears that Apple had several very good weeks in
terms of growth but was largely centered at 0, while Amazon had no
particularly good weeks but had 1 particularly bad one. On the other
hand, Google and Microsoft appear to have been very consistently high.

Similarly, we can look at box plots as well:

``` r
final %>% ggplot(aes(x = ticker, y = growth)) +
  geom_boxplot() +
  labs(title = "Box Plots of Growth Rate by Company", x = "Growth Rate") +
  theme_minimal()
```

![](README_files/figure-gfm/boxplot%20by%20company-1.png)<!-- -->

Here we can clearly see the few outlying months for Apple (2 of positive
growth and 1 with negative), as well as several very positive months for
Google. On the other hand, we can see that Microsoft was just
consistently high, while Amazon was consistently low (relative to the
others).

### Trivariate Summaries

Instead of looking at simple summaries of the growth rate, we can also
look at the weighted selling averages across time (i.e. the weeks). To
start, we will look at a simple line graph of week by weighted average,
colored by company.

``` r
final %>% ggplot(aes(week, volWeighted, color = ticker)) + 
  geom_line() +
  facet_wrap(~ticker, scales = "free_y") +
  labs(title = "Average Price over Time by Company", y = "Weighted Average") +
  theme_minimal()
```

![](README_files/figure-gfm/line%20plot-1.png)<!-- -->

When looking at the prices over time, we could easily form a different
opinion about the Apple stock prices. Clearly, they had a dip in the
middle of the year but started to rapidly increase towards the end of
the year. This means that the 2 particularly high values of growth rate
for Apple were likely at the end of the year, which would be much more
relevant in predicting the next years growth compared to earlier
numbers.

It also appears that Google’s growth might have been plateauing somewhat
towards the end of the year, but it is hard to say without further
analysis. Microsoft seems to consistently dip and then continue rising,
while Amazon seems to be all over the place.

Because of this trend at the end of the year specifically for Apple and
Google, we can look at a contingency table of number of transactions by
week for those two companies as well.

``` r
final %>% filter(ticker %in% c("GOOGL", "AAPL")) %>%
  mutate(adjCount = transactions / 100000) %>%
  group_by(ticker, week) %>%
  summarise(n = adjCount) %>%
  pivot_wider(names_from = ticker, values_from = n) %>%
  print(n = 53)
```

    ## # A tibble: 53 x 3
    ##     week  AAPL GOOGL
    ##    <int> <dbl> <dbl>
    ##  1     1  28.6  2.21
    ##  2     2  47.4  4.00
    ##  3     3  34.3  3.00
    ##  4     4  28.5  3.78
    ##  5     5  60.6  5.21
    ##  6     6  35.9  5.81
    ##  7     7  27.9  2.89
    ##  8     8  32.6  2.55
    ##  9     9  61.4  4.46
    ## 10    10  59.1  4.39
    ## 11    11  51.3  3.95
    ## 12    12  43.0  3.79
    ## 13    13  35.4  3.74
    ## 14    14  25.2  2.98
    ## 15    15  31.0  4.20
    ## 16    16  30.7  3.45
    ## 17    17  30.1  3.72
    ## 18    18  35.1  6.32
    ## 19    19  35.7  4.37
    ## 20    20  38.0  5.13
    ## 21    21  26.9  3.48
    ## 22    22  23.9  3.00
    ## 23    23  19.9  2.61
    ## 24    24  22.7  3.13
    ## 25    25  28.1  3.51
    ## 26    26  22.1  2.90
    ## 27    27  22.3  3.04
    ## 28    28  28.7  2.70
    ## 29    29  33.2  3.01
    ## 30    30  29.6  3.44
    ## 31    31  29.9  5.40
    ## 32    32  20.5  3.06
    ## 33    33  20.7  2.71
    ## 34    34  30.3  3.44
    ## 35    35  20.5  3.41
    ## 36    36  27.4  4.00
    ## 37    37  28.6  3.11
    ## 38    38  34.2  4.05
    ## 39    39  30.2  4.20
    ## 40    40  32.7  5.27
    ## 41    41  28.8  4.45
    ## 42    42  24.0  3.80
    ## 43    43  23.7  4.48
    ## 44    44  29.1  7.36
    ## 45    45  23.3  5.36
    ## 46    46  21.6  4.14
    ## 47    47  31.2  4.12
    ## 48    48  27.7  4.03
    ## 49    49  51.1  5.95
    ## 50    50  41.9  4.39
    ## 51    51  52.7  5.78
    ## 52    52  26.1  3.79
    ## 53    53  27.0  3.29

Here, we can see that both transaction numbers are highly variable, with
Google’s being much higher in general. It will be much easier to digest
this data looking at 2 bar graphs:

``` r
final %>% filter(ticker %in% c("GOOGL", "AAPL")) %>%
  mutate(adjCount = transactions / 100000) %>%
  ggplot(aes(x = week, y = adjCount, fill = ticker)) +
  geom_bar(stat="identity") +
  facet_wrap(~ticker, scales = "free_y") +
  theme_bw() +
  labs(title = "Number of Transactions by Week", y = "Number of Transactions (in 100,000's)")
```

![](README_files/figure-gfm/bar%20graph-1.png)<!-- -->

Here, it definitely does appear that Apple has 2 weeks that stand out
towards the end of the year, but they are still not as high as some
weeks early in the year. On the other hand, Google’s transaction numbers
seems to be highly variable all throughout the year.

Lastly, we can look at a scatter plot of the opening vs. closing prices
colored by company. We expect them to be closely correlated, but looking
at the scatter plot by company may make the patterns a bit more clear.

``` r
final %>% ggplot(aes(open, close, color = ticker)) + 
  geom_point() +
  facet_wrap(~ticker, scales = "free") +
  labs(title = "Opening vs. Closing Prices by Company", y = "Closing Price", x = "Opening Price") +
  theme_minimal()
```

![](README_files/figure-gfm/scatter%20plot-1.png)<!-- -->

As expected, the two values are very closely correlated. However, the
patterns do seem to vary a decent bit by company. Apple has the few
outlier weeks very high for both, while Google seems to have some
clustering on both ends (that are also a bit more varied). Amazon is
much more spread out in general, whioe Microsoft appears to be the king
of consistency.
