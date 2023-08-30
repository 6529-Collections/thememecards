
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:14525       Length:14525       Min.   :1   Length:14525      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:14525      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18018769 # https://etherscan.io/block/18018769
block_hash <- "0xdc46e85c2badbbef9cbc943a29ffb1267eedd552ff397d1e4b492c98cd9e201c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5052 

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
      1 0x019accbe7f0823598ea968b7271f39735016da9b      1
      2 0x11099ac9cc097d0c9759635b8e16c6a91ecc43da      1
      3 0x13bfd26cf7d981aae5f8d4d88af4b34b4b1d4da4      1
      4 0x14b9837715f322b8c515efc024efa2d397b75605      1
      5 0x16906fe850dd6641abf46eaf887b4e8ce0ba9fa1      1
      6 0x1844247cb5320efa99b6804e73b5cae8c1181768      1
      7 0x1af331dc34dd7c5c62af28b5685328318b61888a      1
      8 0x24024f20853aa10267f71f03c7f29b4567b56955      1
      9 0x26ce7a209ef8aeff7f6fc5a0d64b1412553a9904      1
     10 0x2a74bd40df54a0065aeed3cd764ac37325938b8f      1
     11 0x2dc58f7cb47b5bc558b6b0d80a03a921156ba4fc      1
     12 0x2f49954c093e36325af639838b9a6824b988b824      1
     13 0x2fca9a598678d0d09309b06eeff493dbf6c7d1a9      1
     14 0x3457d2aaa857fe5c0574757c00ca8df5c0a19fe2      1
     15 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     16 0x3b02a822021af8ea7718b79717fea58ec3da11fe      1
     17 0x3fda15273e4e7211377352e0bb84c02703303723      1
     18 0x409c9f9dc413d7b026c677512aae81323b2c76a9      1
     19 0x4155ab641b6c36be6822704e82b2b3b5d7e55778      1
     20 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
     21 0x42fc06d238f730b945c89b74cd30758dba17847c      1
     22 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     23 0x49d88ae128da4d3332ed2e4e45212c2142f24488      1
     24 0x4b4446723612db79e7f5551ea0568248a0dd8343      1
     25 0x4c9ab1064da9c0d384530131498e3c34617a1508      1
     26 0x4dd5a4d1fcb6f8938aa8ad070aefe1b19e8f9c0c      1
     27 0x4e0358255ad25c4306fa0ee6657ce0c52ce22f53      1
     28 0x5591f6258fd9eb2ad0d3a4e88245d06e1e8661bb      1
     29 0x573efb51f83d4265763f530295d4afb25a487bb7      1
     30 0x580a96bc816c2324bdff5eb2a7e159ae7ee63022      1
     31 0x58a216fa7cbf9d4f46ce69a4008e628b715651be      1
     32 0x59e891b2368acadde5bab30c8eb1728a1e49c4f3      1
     33 0x5a6abe2b4bd1e178d984afa8630cd6ac212b070b      1
     34 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
     35 0x5cc1e645f4cfae6c00a4f055c12a3d80fef925aa      1
     36 0x5e9c0665630b8659180da179cd789edea40152d3      1
     37 0x679bd1d460b29feed7bf989d1d1f2db0449eb605      1
     38 0x6af6e128fa71de2b65aecadd45e6a6ff6700d0f8      1
     39 0x6f700e6e498bd11e4c65e1adc2035a398990619e      1
     40 0x768057610a07396e05046af01e7c9b912e4a81f3      1
     41 0x77039399801f462b4ed13444a266b16355c471bf      1
     42 0x7b23d4f8aabc412da087aa32249d3ac923683e8d      1
     43 0x7ba876b2019fcddcbf5087c5c0c2f623f79e6b1f      1
     44 0x7bd0492cf724a7ee158c967aacc95bb41fb00b0e      1
     45 0x7d93f170dfd65d14d58682678b7a0d171f287c93      1
     46 0x7e5b399e254665590266ac6a9e2a1e3336576cc0      1
     47 0x7f64c6e6908de72221a06a06b4a02a6546a4d6d2      1
     48 0x7fde0088a3b772ba42ae088e2d3b0b9026e26dd9      1
     49 0x8400b081f5dc9c5bffe1b57276afdfab2dfbf29c      1
     50 0x8b882151997a0e5e6b693e9e0d8cd76a7581f90a      1
     51 0x8df962bb69f75e2388f39297a85a31fbd3fcffd9      1
     52 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
     53 0x902866da6badf2c9ba4c1060eec931cb1ead0346      1
     54 0x903b1b960ecfb37d994fa0cc537777d8ed766656      1
     55 0x90c11d589dfd2e39b2813fc04a1be80ffd9b7dca      1
     56 0x9888eddf5cc6c5bda466a994f0ebd8efb5ee90d1      1
     57 0x9b67e27ae8b613fca9607bd1d2c784fb781433e5      1
     58 0x9d45e039eed486b7ee52df94450eee9c1dedf315      1
     59 0x9eaa362844dbe545b63b8c0bda90fed9ea1f5625      1
     60 0xa0c414e69f7d3281501dc5485fa6bbfc858fd00b      1
     61 0xa31b444f2cfd111d16d61335247e66bc491c9039      1
     62 0xa36f3c6219485d4c779e58f32e4f6f6ac8db33e8      1
     63 0xa52f056c6b8dd39759fead233103a14ddee4be55      1
     64 0xa749aede014231311b1d7272ad64909721f085a6      1
     65 0xaa1c29505160181e493f4b373f0c3206cc2a32a3      1
     66 0xb8bb2fbad09c68aa90c8ad6cbcf33987bb7add38      1
     67 0xb98f24c3e0092e90dc9d5d80cd6cbb1034a966c4      1
     68 0xbc463a622a228d98375d575110c0ada0dd4255b7      1
     69 0xbd5224f9509c0f4cfcc99296dd72196661fa8d40      1
     70 0xbefe5d435616619253be2e448310f70136d0fddc      1
     71 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
     72 0xc35c8d48553f02d0cded9a1eac241c4d628762f1      1
     73 0xc424e4642e0bcc09e545127200ed686d3869ad88      1
     74 0xc68a2cc2a8a9f4c0a81ec2cfef61f7a8e23d715e      1
     75 0xc98d8c153fcf66955a44b4678255ba2174f71e05      1
     76 0xca11d10ceb098f597a0cab28117fc3465991a63c      1
     77 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
     78 0xd62469de1d8da954cd5a054961f1393b4f871c9e      1
     79 0xd73280d61210370d32c3dd05639753ec7f0ad4a9      1
     80 0xd8b9f8a36ac3661670e066800a78882e5bfc7018      1
     81 0xd8ef37a3b1f727deaeb07194b333da4be9c75bec      1
     82 0xda5f988e5fdc51f1a5d72cd6924671709615af16      1
     83 0xdb11536b4b5006557626c9acef2da5fce5bc438c      1
     84 0xdb5008e73c7cefe15a147450352be3cbd258da97      1
     85 0xdf617fc072215c638137b3038628b420064c06b2      1
     86 0xe036a80f628e531982189fb6c4f277deca521e36      1
     87 0xe8f704418f3be2a20d38e3f1cf7531e3bfe364c4      1
     88 0xe93a9c6e2deac33995f7de57ac083a65733f0813      1
     89 0xeb7921cda81164f7e711ac0bec1a9243fd10a815      1
     90 0xeba262a77d5ec21446a45ebc978db823794d3720      1
     91 0xf0280db7c831526f80dae97d2d14807ef889aff0      1
     92 0xf2dca9d0652c43784522397c11b19694c73074a6      1
     93 0xf2f2503d93cc1afd87ae80d38d76cba64249acf5      1
     94 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
     95 0xf61cecb051af46421c79b3457f1e19397cde936f      1
     96 0xf6ad2baa24b7be97921dd359eae00ed0d8a1a5cc      1
     97 0xf7474070f2b9843df17e41213b6d89a1a6648b86      1
     98 0xfb87834945db9862f2cbfd3fd3b3493cd2de2abc      1
     99 0xfcda163ed7c3cc87dd57e2576c26694e974bd85a      1
    100 0xfe4da73ff6543b17b2b18e0e5d94bc87bd79f527      1

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
