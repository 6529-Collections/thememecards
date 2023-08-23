
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:14954       Length:14954       Min.   :1   Length:14954      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:14954      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17968669 # https://etherscan.io/block/17968669
block_hash <- "0xab4010889eb963ee2cf4223177f6714fb40d74f182b447da507b0c09592fada8"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4553 

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
      2 0x13a2b24101f087068bda48c8589c1954509e66ac      1
      3 0x17b4a0de7e92e34520816d27171f6dc8fc407f03      1
      4 0x17c5cb83b05e0f8f678b5817a66cdde7328f3aa6      1
      5 0x188386d433e070826ef3a2ca5d9edf595aa441de      1
      6 0x1af331dc34dd7c5c62af28b5685328318b61888a      1
      7 0x21abbfa7a1ac6300cd9df848784f0012c61cccf8      1
      8 0x226ada63efa09c08c26af852a6f27b91cbbd7bc6      1
      9 0x2616d8d5075dc5e878f40497a4e45924674440ba      1
     10 0x2ac151bb889bca4354e3727b1c356654b8c12469      1
     11 0x2ae1343a5c828cbe5c115785408c18aef387f2ef      1
     12 0x32a0e5b656c2777d0f4d1f834e097d76505c2cd0      1
     13 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
     14 0x362f22ec05993f735c0b9baea18bd73fa99e52fa      1
     15 0x36c246d67dbd8c5c2988091bd6626440db5424d2      1
     16 0x37a51dbd2ba793e42c69ba97a6710bc56e94b56c      1
     17 0x38090615458c10d067dc19b5f1ccf7d4b4dd8bd6      1
     18 0x3840865eb893b283c843d32f6f766526b1ab9d50      1
     19 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
     20 0x41aec789f14e50b2628fc2fbcc29f909d9a97236      1
     21 0x465d06aa6b0dcd4f2d9a38d14b20a042b92efbc0      1
     22 0x46ad4eb64b48c0fda7f13866792a72a67e968e23      1
     23 0x4b986ef20bb83532911521fb4f6f5605122a0721      1
     24 0x4ba9ce953a2247866c899e1d43a5c8e612124e84      1
     25 0x4cd2c410884ee586a8e6418712613f3077aae7b5      1
     26 0x4d207177677fb1128281a54fad993d3736c7f02a      1
     27 0x4d6fb29c0a7028b26f84b694f8ae4cd2ba90f0f5      1
     28 0x4dfd1688ec88704e0d43976c58c34dcdb12ab55c      1
     29 0x5147c9b5f453445f8f47fa9018d7aa85e012eb5f      1
     30 0x52ad2b7e5a8e591cf9ad7124c6dae8f53f87d936      1
     31 0x56a061f4ef706e1f6dcdbcf7e10e4340c1d99bbd      1
     32 0x5c18d25064b9abc166ec38d15408d5569176e8cd      1
     33 0x5e61a6c4d2e7bc93fd15fcf3fa18cc305185a4c8      1
     34 0x5fd22779c58f667cde1e9600576529984495604a      1
     35 0x671e5668e4650cd621b48c0be7582716f9f1ff6e      1
     36 0x6c04e6014067a014a98a5590fe122c666a22a4e7      1
     37 0x6cc4774cf4d4c738e3310f1b210c6ffe23d93999      1
     38 0x6e3258ce4ecbb70e3cb18692d24e82b675d2e2cf      1
     39 0x71cb0bbb26b37e30148203d05ec34ce88fe5c899      1
     40 0x752bd1581c4b9bd8755ec99ff69b119a8e9e8dad      1
     41 0x76163cb574a5d9b08f24762ac6f1fbf33d20bb2f      1
     42 0x76977d13dbad4672fabdcc1b2c739f5230d8c2e1      1
     43 0x7841c102d8d13c1f78c0d942e73117cdc6119697      1
     44 0x7a31e37689852a9a747426c6ac573d43327a7483      1
     45 0x7ab5bd27e41f319d61aa7b643a6eaab9df7f11b5      1
     46 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
     47 0x7f23534387ffbd9c5fff4868c34be54c69c684cb      1
     48 0x7fde0088a3b772ba42ae088e2d3b0b9026e26dd9      1
     49 0x808a023b72260170c95d831f589a1ae0dca1e43e      1
     50 0x81845ec7df4fa5776998053457b8c4fb1e60cf84      1
     51 0x81ad1a19187f0f3444c0f8bef98ea49c1b9fbc03      1
     52 0x8577243a29f7dd5780ab8843b00a2710ab410e87      1
     53 0x89cc08700dcba9d4bad5295dee2a111b90b39917      1
     54 0x8b882151997a0e5e6b693e9e0d8cd76a7581f90a      1
     55 0x927705d26bd2aacfd0ed7492e8e21a86fecb4d1b      1
     56 0x976828b23af10f75e94133b15f31ffb054572c7e      1
     57 0x99fc4dd947bf9569deee3710b28fcf26797c6870      1
     58 0xa00bbf3f9272329710a36fe3f47cb053e377cdd3      1
     59 0xa1709ead88b791f463ed8075d5bc1c2a85f8f183      1
     60 0xa3861a789d05597bc046e8e6d5e10ecde947569f      1
     61 0xa595e9525b0b2dbf91054689e385f5611207b9c1      1
     62 0xa7b5dcb4ea9a2d9db2d0880293ce7a83dd5d6bfd      1
     63 0xa940409938a3a708bdb756b03b29fb6cc26c1a87      1
     64 0xac6fdcad572b38e9a8f272f10c98e5842b91da4c      1
     65 0xb33dba37a572861bf8d6d6237dd4fc048be5410e      1
     66 0xb471a2f9cfb18bc508fe1464eefd585344696b5b      1
     67 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
     68 0xb71192988aa3759bc852455ae92128a619b97f30      1
     69 0xba671412b66993815826399549d18f2c357e336d      1
     70 0xbfe6ab779517544df703159ea41e7d8a71db6ef0      1
     71 0xc3424aa08c0e94e1ece3af84f5767acf8d76c39f      1
     72 0xc37016f3023d0218718ca22a423d9dd00eb58275      1
     73 0xc4e8d1717c3921454792241ab7ba481f1c5304f2      1
     74 0xc76d207ddc2165c884d3865252085dd8caf42648      1
     75 0xc82fc8b3de50b29705a85f912980ec476d0852b3      1
     76 0xcc811ad1fd46ff65df9e6c6dc0181874e9957707      1
     77 0xccffaf47449abebfd00a55aa34eaad5752f6f32d      1
     78 0xd3e2e76e4ab0322614fe7aaa3be0b04bed83d0d0      1
     79 0xd72508badc98629e324496180322e70ed2e28ee1      1
     80 0xded440a3b8d76c828db6737c4368ecbd374c2237      1
     81 0xdffb2d60e5ecb233d448239988d3a6fb6470a8d6      1
     82 0xe1c1c3867181cdb15f5c011120b4b7abcce91c5c      1
     83 0xe1d8963bead06af2f756542b5bb673751a39be5b      1
     84 0xe428d30c12b2b976d3681a17a9db4a185b17db2c      1
     85 0xe47000d2dfaddb0180cbc07da3e2b5c66a612b63      1
     86 0xe4b5ee34deef84754d4d437b2a4f832e24bdd177      1
     87 0xe63586f0ebe319d338b60487243021be9bc596d0      1
     88 0xea02975ac761a8fe34d3a8b29ecdaa7cbf766c61      1
     89 0xeea254f9d7f096ccab5911927a0550bd1e332909      1
     90 0xf020a364d81900e8fe8f0e53b0267eb88d0c05e7      1
     91 0xf1d83de0ead95747f4ac184b36659c0d538309c8      1
     92 0xf3a80df4e39486545b2c3e79a79c8500dbd71ae3      1
     93 0xf3dd450b30958c820781873c3287ace1e4424cac      1
     94 0xf4a2f16ea9ee6b69e90df8d8cf5a5b7622fc89ca      1
     95 0xfaaf6e583546295ce96316ec26424f37885668b9      1
     96 0xfae772e7498b0cdfc4ea929da72333dcc0dd2517      1
     97 0xfdec0aabcd16f5e3953aa479d6d1dff298483c1e      1
     98 0xfe0fb970566e5ee272692ce8b831f46317de639c      1
     99 0xff9ee049afa0389e6358a6a20548dcb0cf7a12a9      1
    100 0xffb8a15dc8796af3e7bec74e528f1bd810b854ed      1

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
