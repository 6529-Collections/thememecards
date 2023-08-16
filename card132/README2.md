
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:15618       Length:15618       Min.   :1   Length:15618      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:15618      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17918669 # https://etherscan.io/block/17918669
block_hash <- "0x1304a79e219c9616e3682c327bcd354fec452caca8e1ac22f6469eede3038794"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4610 

## Code

``` r
subtract_multi <- function(x, y) {
  for (i in y) {
    where <- match(i, x)
    if (!is.na(where)) {
      x <- x[-where]
    }
  }
  return(x)
}

pick <- function(df,
                 contracts=c(),
                 address_remove=NULL,
                 address_subtract=NULL,
                 address_max=Inf,
                 address_pick=NA,
                 address_replace=FALSE) {

  df <- df %>%
    dplyr::filter(name %in% contracts) %>%
    dplyr::filter(!(address %in% address_remove))
  
  df_by_address <- df %>%
    dplyr::group_by(address) %>%
    dplyr::summarise(
      balance = ifelse(sum(balance) <= address_max, sum(balance), address_max)
    )
  
  pool <- df_by_address %>%
    dplyr::arrange(address) %>%
    dplyr::select(address, balance) %>%
    purrr::pmap(function(address, balance) {
      base::rep(address, balance)
    }) %>%
    unlist() %>%
    subtract_multi(address_subtract)
  
  if (is.na(address_pick)) {
    return(pool)
  } else {
    return(base::sample(pool, size=address_pick, replace=address_replace))
  }
}

tally <- function(x) {
  if (length(x) > 0) {
    unlist(x) %>%
    data.frame(address = .) %>%
    dplyr::group_by(address) %>%
    dplyr::summarise(
      amount = n()
    ) %>%
    dplyr::arrange(desc(amount), address)
  }
}
```

``` r
base::set.seed(seed)

address_remove <- c(
  "0x3a3548e060be10c2614d0a4cb0c03cc9093fd799",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=100,address_max=1)
```

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random100Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00baeaa84510584a6e4624964b75321ddb6766cc      1
      2 0x010d09ddd06776f28361556f02d3cc137960876e      1
      3 0x02f3f238f5a6521b4715bdc73283d0fb06d276a3      1
      4 0x0441905229e772ed05692db0967749164bc52417      1
      5 0x107752288b215467de25c16119787d715ec8e26e      1
      6 0x13069eaddf4d50ec08c94e831d7740e756837f57      1
      7 0x159968fad6a3df074cfe7e587cde0c5f375c1b70      1
      8 0x15c8d7d32fcb740eead55cc96afd2fa05cbe6e7d      1
      9 0x19487393409ef4096ce5cb287f17d52b96e37c8a      1
     10 0x1b90f6e0d42a8acbc94d25d53a242a2a9c85254d      1
     11 0x1c22e514203bd6157c7db84c616f3746bd03b431      1
     12 0x1d8a9e8d00f6b7f65dac2078475119bc02da0682      1
     13 0x1fef4e18daee931b3ac7a8ab69546c4d511d8ede      1
     14 0x26a6434385cd63a88450ea06e2b2256979400b29      1
     15 0x28b91b119f139a18b5688f6327c4c4eb839ed644      1
     16 0x28bd50226a8523ccfaa2ff2873eef032a7feeafa      1
     17 0x292402b58d40393db796380cbacdc1d9d9971beb      1
     18 0x2929e4c81c4c72a1d9371b09b0bcda2fa593ab6a      1
     19 0x2a3438a9af1fba7b5e63a6f333fe296ef749067e      1
     20 0x2d12efac1f666bbeead746b3755905f817ee611b      1
     21 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     22 0x32d3aa5df3203ac5613cd5b22c642929e3dae423      1
     23 0x3680eaf1f85bec9f120bcaaa9ef469fb849e1781      1
     24 0x3b2dd499b5c4fdf97f794525a9586f62e334622d      1
     25 0x3d7d85485b37360c7ad7ce17e549fba5473a0c0c      1
     26 0x3f7dd0b106b10d06bb4813715b4e24d9e626dd64      1
     27 0x3fd78bea07849b766742180198523982ed30112a      1
     28 0x423ce4833b42b48611c662cfdc70929e3139b009      1
     29 0x424060738dc814b16c203650ba7e1ecffc7e504e      1
     30 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     31 0x4a6eacd00e3a586550bb3b52731d51ce7f53f490      1
     32 0x4ff05543d3a53ce52da701fba2af11255f0a066c      1
     33 0x5054a665b7ac3c30939b02acb34827af25aba35d      1
     34 0x50e2a2e9ac6aa66b7d54517ee02b89bcb58df934      1
     35 0x5286bc17220d51a36e55f5664d63a61bf9b127a6      1
     36 0x5322d25a83fedb9055fbdfaf3e485821438871dd      1
     37 0x563b4e3be5452bd01852dc5e698ca4b2392d1200      1
     38 0x57f138b1e9c859151533f4dba90891eb4971d319      1
     39 0x596558fd287175afa7ff695f565f65e38547b0e6      1
     40 0x59689fe7544e73c45c4b6219b1557cd4aee020a0      1
     41 0x5b067f845fd99cf1e80e2b5833bbe6e6910f6843      1
     42 0x5e451b2d167c2a8faf0a8b23cff27996a28f28bc      1
     43 0x5e9c0665630b8659180da179cd789edea40152d3      1
     44 0x6072b510aa765dd2f31975c9aa65dde39fd1a282      1
     45 0x6885c0a837c320f47a66df243c49a4e605ceb1ab      1
     46 0x6b1cf6d736c107630c40c7ba7bb652821f6dd6bb      1
     47 0x6c04e6014067a014a98a5590fe122c666a22a4e7      1
     48 0x6c5d86dfafd74031ea5ea1c7315c8010304df411      1
     49 0x6e9075da365be17041e704a16d39652a7a54b862      1
     50 0x7193a5e5dc30b70dc5e434ba2144ae76e33c3eab      1
     51 0x74fb3bec1545e02fa63fd40561f7bc36e2c06134      1
     52 0x785442cfd567214856fde0906774574a8285bb67      1
     53 0x78887448976b93443125cd9ec40a9485684c759b      1
     54 0x78ff371955e3a7af7e83873c9775cf382b6a0cdc      1
     55 0x79b5259dc0fcb44f9751bf9f78f5ef46a7958460      1
     56 0x8488f78d943cf6b5e1231c5370fed186ba7a3044      1
     57 0x89b5deaf887ef9e3360f5e3347e6bcdd738778ba      1
     58 0x8dd62416fa3c6efada57eb7a6383885045390204      1
     59 0x8f9be4310f9abb0e5843cc6363908c9b01dfeb3f      1
     60 0x90113cbc08169e8f9e58debd3fc36de3135eebb5      1
     61 0x90c65efd30cdf57e72c9b394b6cf36707135ec7d      1
     62 0x9eea822800921cd4425d86f0e797749b5220ce0f      1
     63 0xa17b595edcb5c66c532c830e34e823a3e0033c8e      1
     64 0xa266b9edf05b39a6d2bd9efbe1d5445192bda55c      1
     65 0xa2b1db1d846d368489b7211831bedb50b94ae83d      1
     66 0xa2d1ebf3b7d49cbc5b171f4466b7a90f75bd597d      1
     67 0xa2f26a92794488a6f41970108e267e1d67957652      1
     68 0xa36cbf2a4488e43a432a38807a7a4b1c21ea11b1      1
     69 0xa495e370e5987babb57580319c2ecb8e52a1239b      1
     70 0xa55048cc0369e412cf7db3d9bb30afb51d091a12      1
     71 0xa56131d1c79a35efeedd812bb521b81c6712e407      1
     72 0xaf44539ba68c318a0de5e1d7899a669f374e180b      1
     73 0xb33c6c9269e78f5c636f2c679c435c7c817f5ed9      1
     74 0xb6941a32218bd6a34ca3d80b6a9ca20b75e50a6d      1
     75 0xbab3c738620527f4614c0dbcf18411328555f24d      1
     76 0xbbc30ddda94ac46496736215129b6b6f8c02bbf4      1
     77 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
     78 0xbd9b7373aac15d9a93c810df3999343f4fe1ed88      1
     79 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
     80 0xc7385fdb1174a95fa43d8f77e4dd283c6cc4bbb1      1
     81 0xc7d178d2582a0889d3ed5926ca5ac7c9d2a97317      1
     82 0xce27b1362f6a109d063b6c7e81ac9e30cc79d547      1
     83 0xd09fbc94b3c830afc086733632ba0b63a9a44320      1
     84 0xd60eae7735e99c95aa8add97a828876fad7a4132      1
     85 0xd7597df2a4e40fffb742ee7c119cdcccb6841940      1
     86 0xd90fd51c9fa7d51b9c53c158231ff8498ede1140      1
     87 0xe10b1da7327e618865ff3ac19c36f6ab9cadf899      1
     88 0xe553fe5b71a236a8d6b03cbddde8be48c5fc5402      1
     89 0xe5f33c3d6f158dc9a29dd90926b8f69d3dc4f10b      1
     90 0xe6e4d92009406d08851c2e65ce6dd324ad76a87e      1
     91 0xe7ee5a3a7e0c9114a0d39f53619bad9ec7466068      1
     92 0xe8f19812dea9a836125bec88ed5ad9c3436dd3fb      1
     93 0xea94dace59ede50c55d53cc90c26d8ec4ff0b685      1
     94 0xee34bbb4882c0ca569f70901217e6aeb3b347b62      1
     95 0xf07f65cd74becd71339fa95bc10c3c7c0a070d0a      1
     96 0xf2626eadce87913c29e63b95e39552e1bbe26b44      1
     97 0xfb1833fc48c9511c19d3c5f12e86f7ed28993bc1      1
     98 0xfb38cc51133ef30286229be866a261c36c1e8f8c      1
     99 0xfecf9b009af216056d27759c1489f00fc62428e2      1
    100 0xff6899914cc3ed62c31920d85391ada61e6cd4f6      1

## Versioning

``` r
R.version$version.string
```

    [1] "R version 4.2.2 (2022-10-31)"

``` r
packageVersion("tidyverse")
```

    [1] '1.3.2'

``` r
packageVersion("magrittr")
```

    [1] '2.0.3'
