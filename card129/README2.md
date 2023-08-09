
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:18748       Length:18748       Min.   :1   Length:18748      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:18748      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17868669 # https://etherscan.io/block/17868669
block_hash <- "0xd55111db3ccdb8ed8b7d3fdc54a4a3cf7c0b8a3b2c0c7eeaa3b3e863980b0d80"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5009 

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
      1 0x0240ced4de4edd91884027f2e06a1780331fab74      1
      2 0x03285482fa5114dde09084dbef4e5c4f8034cb8f      1
      3 0x03b367733f46743534c9bb13bd4fb5ae2780babc      1
      4 0x04547314afabd1060fe6513518584b1ca602b51c      1
      5 0x0578aa0595e0c83530dcde1d7a339479bfe6b0b7      1
      6 0x059793939effe3cd98edaffad580abc181b1a96b      1
      7 0x0da8ec9829af1b58d247078e398ce00420ddd942      1
      8 0x1418b42e167a8aa3bea88f76a7889caf2c0436e5      1
      9 0x14baaa573906c24ec9dbdbd3d092fdb869b2d0f4      1
     10 0x166b8de4cf92d6e1da50d2811b7dc58c76b73072      1
     11 0x2023e02221550d3d75fbe33243948ea76a09a6c3      1
     12 0x23546240effa073e8e23caaa7b41a6c525a7da5a      1
     13 0x29bec842430930de7a8a1a6d95e739ff824f2c08      1
     14 0x2d19d78b7172464f295c200b18225f566899f2e6      1
     15 0x2e769f9cb238e5d93fbe353831c80bc92ba239d6      1
     16 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     17 0x31eae19c7737925fad2ff2f007ccef3c9a7e2239      1
     18 0x32a0e5b656c2777d0f4d1f834e097d76505c2cd0      1
     19 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     20 0x3433faf996a75d642a1e8dc82b8d70381e252881      1
     21 0x3bc3a258fc6dd9f6d3021a47b44706c656faa6da      1
     22 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
     23 0x421ae97f38789280fd114478a748a0f943e6bdd9      1
     24 0x423ce4833b42b48611c662cfdc70929e3139b009      1
     25 0x46622e91f95f274f4f76460b38d1f5e00905f767      1
     26 0x47a366519eaf385804a404697eb8d5cb1fd55d8b      1
     27 0x480480309057a072daeda781ec15aadd61677f13      1
     28 0x4839f0a75ede31d26613c7cdccba89be774e9856      1
     29 0x4874211da929dd87e28a4267c4b96a5d0ccf48a7      1
     30 0x488b9adf17ba8c664202d22533f3866d8d6e9e97      1
     31 0x4909c45bce6c9f11081ee53404b866aee9e0ddbb      1
     32 0x49a606975f17ec67da1438e6be5309c112d4f4b4      1
     33 0x4b85b8e5d4d2e3220218f69500c4804721ee4cfc      1
     34 0x4cd2c410884ee586a8e6418712613f3077aae7b5      1
     35 0x4e0bd3bef962d9fa4a561b288d6680faf8fcd200      1
     36 0x5062643314417a88ae40bbdc8b15f21c4abb288b      1
     37 0x536d83e66759cceb0d3b5d6b70d61696404ead7d      1
     38 0x552f294781d4313fe2f5f6b8df3ba90743a6e17f      1
     39 0x580a96bc816c2324bdff5eb2a7e159ae7ee63022      1
     40 0x583b7fa299d2e40c235f34bbb16e7e8fdd6d379f      1
     41 0x58c5b881fb2794d04f0dc0ec65f94a119cd43aa0      1
     42 0x5cc1e645f4cfae6c00a4f055c12a3d80fef925aa      1
     43 0x5fb77d2d3b3dd881f5d4bc6da7a0f18fbd694463      1
     44 0x5fbe0ae423f1767028f150aa92545267507588ef      1
     45 0x60dd7313afa4a59a78cf885be5de99c594d6e49d      1
     46 0x63452484ede33876ea5e18f7c549cf183e31d352      1
     47 0x6509b2f0b69747590c2d37f019cb5840d82349db      1
     48 0x68cbec6e76a8cc835ae75cce5feff1f36cbaf764      1
     49 0x69419b29e25dfcb0f1f27906cadfa332fc145d7f      1
     50 0x6b1368d3a890c46ce9b0d6eac4d36f2a432904b8      1
     51 0x6c4b1abaa331aafd5020b0e865a9bdd567629327      1
     52 0x70ea6798510298b2eb333ebe3fd63f5959149444      1
     53 0x70f4a2ce11a2524b6686963a5de86e336ee56ebb      1
     54 0x7239eebf5f37aad21ca7cb20c882290328c7a8c6      1
     55 0x7bc7620d0d84ef434cfa527e0716ae5716e24b02      1
     56 0x7d462af7fb6aa7b5b91fd5ca5bdabb7fd6124b26      1
     57 0x818859ff6240c09b5ef9cb1f60f6f8555f6bb49c      1
     58 0x82abb5df200d2998db4e80639b145d052ca40062      1
     59 0x84cd67d2bcb28694b7b02ccb9736bc9547181588      1
     60 0x88d8fabbb70d3854b7be95a0237d957cbdea24c5      1
     61 0x89e8493a292aa54734f7c2ff4765719138f18189      1
     62 0x8b044b599fb17bdf3c2569343cbe9dcbd215a323      1
     63 0x8e5f71dc1d9b1dd976137d61fadd5a4318a8ea39      1
     64 0x9168fa36b033006090a9159e9ec47609d84b1e45      1
     65 0x92df93709383a40e5ee70e66da100a7aa850dfe2      1
     66 0x94fb300b3796bd07bdda0f3bf94cf17952a0a11a      1
     67 0x957a85e0f23a5c6e1ca7c8e869fefd01cc406e0d      1
     68 0x96ddb748d3c0a24bdc57930567bec9b591ff5b11      1
     69 0x9760fed718c26df146e0e47e259609281e0d4954      1
     70 0xa84b6da113c0279363dd1dcc7465677a4b0455a9      1
     71 0xabfc4b16844d99c9e0c8658858b930659713eb23      1
     72 0xac2c874f01914c89601abddc6320ee36bb396d47      1
     73 0xad887834294a92d5d6590608de07002be6fa3384      1
     74 0xadb02d3da1566b8970c73d34e6efced87fae5981      1
     75 0xb50495766a322b105aa8025dc07cc75d9403bb5b      1
     76 0xb576690181b482427dbaac8da0dd5dd7b66a20f9      1
     77 0xb8903d33924b10b9cda7fd9505e395f1ae0b8532      1
     78 0xb8a1277b827d69ff3626f0853c370ebdfb035ac5      1
     79 0xb965603c7d99ce1dacd91eecb8ca9c0d061b4c60      1
     80 0xba0995c52474dfb7c48181f71454a63a2847982c      1
     81 0xbb1d6ee66888123aeaf75349a6e6026979519acd      1
     82 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
     83 0xc20490e8306257df636e23be02a291b553f341ec      1
     84 0xc422294fc110d20e70e8fa2e09efb930fd392b88      1
     85 0xcb9f87919b6818ccf3e8350e0ee2e13200dc8fe8      1
     86 0xd0f6805f43f002229faf7c6676480bab015cd6f6      1
     87 0xd37697f3c50d7108fef25d16cafe229753224a05      1
     88 0xdab22cab9d6bd85872c7507b9470ce9727aa22e2      1
     89 0xe3262eede42a5eb5189b7a57443257d35f7fb5f7      1
     90 0xe760bc1d8b3a8cda9ef23708bf10aa1c530119f6      1
     91 0xe859ef79c40250a4554a35c84794ca6c80516200      1
     92 0xeb193090edbd341f606c30af743b48943b750272      1
     93 0xefbfc695b171ec729ac92e7e2b0dbdde3fe201c6      1
     94 0xf5f01873a719f3c04c01b12f849226226981a777      1
     95 0xf694d1bd527b09e030851062839d57a44a5b565e      1
     96 0xf770e20248ec7cbe10d4e9090931171883d13136      1
     97 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
     98 0xf993d5474cd607e26b57e1de1556bee36de2d0e9      1
     99 0xfaa0b3e499e752c409439d7c02fb60547ab241eb      1
    100 0xfb361a36fca7e92fdb47c40df43713f68044ffb2      1

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
