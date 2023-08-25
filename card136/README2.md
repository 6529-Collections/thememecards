
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:14947       Length:14947       Min.   :1   Length:14947      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:14947      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17982969 # https://etherscan.io/block/17982969
block_hash <- "0x3ee0a74f08880d6f9fbdf8575d0ae3377e3e920f1d720368d013495646d364ba"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4524 

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
      1 0x05b00d69d50426020749e1e1c60901e129d3e43b      1
      2 0x073d260eef1a0299bc04d82ceb5501a492e08f6c      1
      3 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
      4 0x08132517586f388f8b99d842cc5b3feaec776836      1
      5 0x0a8da356dc80da3a114d63e752cb42923b124019      1
      6 0x0e0b904e2940a31576326d2f335564b3c85a8afe      1
      7 0x0f3ffc89b30f5561d49c11021d5f7164c877c2e4      1
      8 0x118638c389c7148017cc5b85d1f6d1fd25f76285      1
      9 0x197b380ac62fa4785067cf7257c05f052776039a      1
     10 0x218cd7a6ecafc8dfb02456a61539fde5dbe5d22a      1
     11 0x21ac878dd5db6ba349a443d37d1a1a36844e2800      1
     12 0x26631dc0506a8451646f7fdda599141f07273205      1
     13 0x2916768f1fea936b6c69830b8e1e3bad5e612255      1
     14 0x2c673dc2eefbf4c95597f3953a55861e38e3cc80      1
     15 0x2cf84928261f655a47d04ec714d3bedf9375de46      1
     16 0x2d2052be503780c575629a22aa84990f9e38a7d5      1
     17 0x2ec8b02757df2a153f2665d09c88acc0f9a2c6c3      1
     18 0x2f0a7eeca5747c9f8f2fd3257e42cc3a5a5d4cda      1
     19 0x319dddc9b8ca2f8717b1b2fa7db540e3290f049e      1
     20 0x380886e656ce40bd145a9fb85ac7e3dc51d5ee8b      1
     21 0x39dfeed68d0c848f3927cf8d01663ea639ec5b3f      1
     22 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     23 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
     24 0x3f3e791194fb13723b2bfcd663057d4ee157c30f      1
     25 0x41a7519f4e3d8f0c93ac9e8cbb413763a9e213b5      1
     26 0x47029dc4f3922706bf670d335c45550cff4f6a35      1
     27 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
     28 0x47eabc96c1825826b7cb176673e4bb936ac709b2      1
     29 0x48a3c3187975058eabe2d1bd7535d4a26e3250ba      1
     30 0x4cb086a121e0bff03ba0d0a2c57b15b6df399ab1      1
     31 0x4ec89ba6558ef10b3435246de77aa3bf8f2ba745      1
     32 0x5514a08f657685ba7f1990299da1da1fe9ce44f7      1
     33 0x563b4e3be5452bd01852dc5e698ca4b2392d1200      1
     34 0x5688eedc08e9e7301c1d7102c95112253b393e1e      1
     35 0x56c31c02e723277750739f04963610bdc53ccde3      1
     36 0x58b99639cf3b4c7f454a337168bfb070ecb37a04      1
     37 0x5b956d9ae01870d463cf0ee364db00eb995c684c      1
     38 0x5e03049843e8ec127694be340ea6f864f315a5aa      1
     39 0x5eeeb64d0e697a60e6dacd7ad9a16a6bdd5560e2      1
     40 0x5fbe0ae423f1767028f150aa92545267507588ef      1
     41 0x61b4c75ef32e93d14edff6c1a344ee8d66a35951      1
     42 0x6206d5426af834886d1d424aaec7a1e03478e702      1
     43 0x6278e4fe0e4670eac88014d6326f079b4d02d73c      1
     44 0x64893c4bdac30961a1e8fea90704dfbeb7b147c6      1
     45 0x6519f68ca26c7f59deeabce7194da4d14e1f2847      1
     46 0x65673f86614c6f1bbda498b9b4371e79de394783      1
     47 0x6945590e2cd384621bdb463983d0d1ee1aa82387      1
     48 0x7106e9b712fa396941a023186011ebbd7d0c201e      1
     49 0x76977d13dbad4672fabdcc1b2c739f5230d8c2e1      1
     50 0x76ba8674f08b13d6f78d74aa00c594cc9ed965d5      1
     51 0x78f5761b5a541c2e0f7d1921eaa14a0546f41396      1
     52 0x799c5516c59312f229af008c3e09eacfc37dd5b1      1
     53 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     54 0x7b858de2d9f5f5f7ceb2b59a2a6c0638a6507c00      1
     55 0x7d340cc21c26b843f3f0670b4964e32cd1c4d7ba      1
     56 0x80b946cf5052620cd1e227ee501aa8f1cb896df3      1
     57 0x8436e9a3aed9f8d595684edb1fa7ebd6ea6a2cfa      1
     58 0x85bc03678220009e82206ddccbf713228750f759      1
     59 0x8811a003b0a2b311cba5da4626a8143fae7a5a28      1
     60 0x8899d0dedbb6ddd065e0fc1e7b3aaed9bc5d0ebd      1
     61 0x8a01a85f1962938bbe6d19266581eae9ed33004f      1
     62 0x8a0ba098c376e331d6fe74875ce242c47d99a226      1
     63 0x8d32eac23c716357ec7a03fb3f27861e45dc6d9a      1
     64 0x8fb6bfb52a285f805dae5bb2ed5a97273f272d18      1
     65 0x90113cbc08169e8f9e58debd3fc36de3135eebb5      1
     66 0x93a781d6e6a8e8916ea766a40c46ed2064b55141      1
     67 0x93eb95dad68de034fef03da64ffb3a70df563a29      1
     68 0x968a9e8b72205c76c6b2a0701e58bc7165f7bba6      1
     69 0x96e8682a8a418e5d610090285bf9d6732ba3245a      1
     70 0x97e3fe76e67640b5327b406b809678c8bf5d2bd4      1
     71 0x981a7420150d815147a62a93905e0064e8ad7342      1
     72 0x994b2b32530c95cd4bdac0ec0e0b41b187385b94      1
     73 0x9aa2b4782b7cf35b7dfc699604a4de16d80adfd6      1
     74 0x9da6640321b9c7a538b5b126e2cfbdc6a999144c      1
     75 0x9ee9a7e08e78d6a9cd9f47ef72592791c0a5d174      1
     76 0xa0c1bca2da31d84180114965d2f80e37b63030ec      1
     77 0xa1709ead88b791f463ed8075d5bc1c2a85f8f183      1
     78 0xa88220a76054be964c7d0e039643138a16ea17e1      1
     79 0xacc0e80e3cccede7d2948ab542700bb9499d2623      1
     80 0xadd93b1acdfa3e865bd1e6495182e9f1ac5cc35b      1
     81 0xaef0f7d5e749bc7dd4303ff5990d1190a3e33969      1
     82 0xb01ec69454206dea9832eb6868733e406c91ba05      1
     83 0xb244f4b768208ca90f7cd92c347ee017b4ceb44f      1
     84 0xbbd38f1c8cc4e8f1c4444db21f4bfe2f6393d9ce      1
     85 0xcf9799f5292bf594d1716e941c67bf77c29e1a8b      1
     86 0xd0f8293536d112b2f1b7fc7adbbf5e54e83dc3bf      1
     87 0xd1f11145770b6d504dba8497b641369fb2e4dce3      1
     88 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
     89 0xd803f85b52e359ca7151336be08c7d22683d0a7c      1
     90 0xd8261516087b76e1027019da8756d85d8ace7b1e      1
     91 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
     92 0xd9ba239a881f718072e57d9810846c8d705f93a4      1
     93 0xe0d209b7324d982bacb757f724ebe00a9792ac28      1
     94 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
     95 0xe910e40a6df51666440a5d19a5b92b6c9bb6064a      1
     96 0xec8982195f339e202c0450c7baa82fb201003222      1
     97 0xed8e0580629d079fb5242e4dedfea3687b685527      1
     98 0xf6f0667a6b09a119e065925a4795d2d4e69705ed      1
     99 0xfade41403a1eee72d805c42f1eaf5f53a54d0e0d      1
    100 0xfece3a78855b52db21bf868e0141927fc7e0d719      1

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
