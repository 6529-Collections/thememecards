
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:20096       Length:20096       Min.   :1   Length:20096      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:20096      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17804369 # https://etherscan.io/block/17804369
block_hash <- "0x6d8919a904bb77d5afe3be45016f84de7165d40d2aa35fe1dfb18bb93ed47780"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4758 

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
      1 0x00063ddb30be7bc2292583d5f143e9d6e6228440      1
      2 0x006bd95bfe6eebcaf78a8a61e10de8f984a70b12      1
      3 0x025fdd585f03ce846740fe5542469f1de425e439      1
      4 0x0637213177486b93a4198719a49751d77af0b43a      1
      5 0x0743882fa09a0c257b18d53dc1b101d3f32a04e5      1
      6 0x0763e484268042be6e5743c731a7adf47a48ec19      1
      7 0x08a9ead5bd9af49a1f777a9f15929871abd684c3      1
      8 0x0c3cbea1a8061f5df8d37a2c152874d36580320b      1
      9 0x0f25c5ac5cbed4ff15aab4ca3639f87a453b9ab8      1
     10 0x13722725d7ed86b99d5f7fe5cb4226b6e42e7d41      1
     11 0x17af4bfcb2e4d9611cd65a4a5c662f95c7167026      1
     12 0x188386d433e070826ef3a2ca5d9edf595aa441de      1
     13 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     14 0x2257d2af0f1af5ea7de2a01807ab173dcd383ef2      1
     15 0x231a99192dbd87752e2f483c0ecbd4b71ea95c67      1
     16 0x23afa861ed353e61b7f4ffe65204a4b36685e5c8      1
     17 0x25b12eea057708adcb521ef7da0d4112523372fa      1
     18 0x2a3e07fc005c26efeffc84c29f38bb14c3dbbb84      1
     19 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     20 0x2db0763686a3e10083b57435c0c8a1049ea66d10      1
     21 0x2de5ec82bdb8bc7662e9994fb24ea6e6f79d9d69      1
     22 0x2f39d00a1fc21f2dc9eb5671147c7ae98f254b6a      1
     23 0x3564ce014967c81e25f6349124673cf60b7ac572      1
     24 0x357fd6924cde3df99a9e83f1ace7a9d85cb18aa9      1
     25 0x3597e01b49f8e6517d661a4924c6f6a4c232848d      1
     26 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     27 0x36c246d67dbd8c5c2988091bd6626440db5424d2      1
     28 0x3701b0bd69b71ffe298f3e226b5ad372b22f0f16      1
     29 0x39c885f14fae8ba96ca233abdb9c8368d0ebd8a8      1
     30 0x3cc8837cd778d12021a4e7baabab0e06d1d1fed2      1
     31 0x3d1805699ff92f10384f6deb5ce39cfbc93021e9      1
     32 0x3ec70d39ed50bfc9fcad534cff646dfd798bc7c5      1
     33 0x4158406b55313dcd2265b42ca0576a9075a9aeec      1
     34 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
     35 0x46bc4aae45feba36eb29ee91527dd9615911eed6      1
     36 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     37 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     38 0x4c85ece5bbf191d3e1b76934c8372d4ae3bf6927      1
     39 0x5bc5d16383c74c0e3ff3b77046c7a4cf17b42e5c      1
     40 0x63f4c36bd548c3ad3dd18364c987e9a062a330ce      1
     41 0x66a365135c7b94c26fe598142733be1de79d37cc      1
     42 0x688b6650f9e1b5d766b5161c0ea3c56c19c95adf      1
     43 0x697e9c6783e348ac67d2c129b6b00ad616bf74a5      1
     44 0x698b7dcb16b624f104b37a2578acaede89f37fc9      1
     45 0x6d0f71cb84f78b3c1c44643733146cb2f9bf9e38      1
     46 0x6d480e89a4ba91fcc27024a658543c8452d4258b      1
     47 0x6f2b6ac9c798abcbe5b5e313f41d401009f2bd95      1
     48 0x72c293eefdb532bd448e876174bd54161f984096      1
     49 0x72e680156324b5afa5d0d84b621844424a8bce42      1
     50 0x78eca41c3085bf0eca6470ab202f51510e5f86df      1
     51 0x7a79944ac7e770cfd13ec024a4b31b8c5efee60d      1
     52 0x7b220358c3ec9ec03b08120dccec56f16fbf33a4      1
     53 0x7f3b24d4ee18c5f92cf83894a954737fe5752fad      1
     54 0x8036bb84031e60a1bac93078c0e59bbd5e485db9      1
     55 0x812010bbe6652497d33c7a1fd986f629769ea9cc      1
     56 0x869b65a919c903e9b4c3e3f9f47d6939ed2a01e4      1
     57 0x8976621ddb09d0e19e1d23eb69e4e8bdb5c1ab57      1
     58 0x8af6e15ed513b5b73573f58158b1b0bbd5085ec7      1
     59 0x8e9be9e56975feb2af4ab5c609343472ebb80bc8      1
     60 0x913735e6d76b6b954ca799511244ff430cda642f      1
     61 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
     62 0x9547dca1cadb86e6c3d80811de4ecc5b7ead2b1a      1
     63 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
     64 0xa2321ce998844cac8a2f67a481fa3e23e4c6f039      1
     65 0xa54837c8f04384b89d9cc90ae368dfc51b77e2f4      1
     66 0xa7bf924a81e531183a81235f2f8916e191cba407      1
     67 0xaa1c29505160181e493f4b373f0c3206cc2a32a3      1
     68 0xaa43b3ee536455939ac6155993351a9b34f72ced      1
     69 0xab25a2c1a37e1f1fd689a8a9ea61eddee054f0ce      1
     70 0xae1788699d4c34808d1d56479c618f2c024a5b52      1
     71 0xaedeafe1ecf674c40763f883be317ae6cfa0dfff      1
     72 0xb2682c854a589f4a034f67bdfb60dd8de6026adc      1
     73 0xb407763f587f5f8726dcc9eeb7a72c402117016a      1
     74 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
     75 0xb6461bf5223dfe357745895ef4473024e9dc2e20      1
     76 0xb86ea3223fa32f1bd7f7ffe9dec2f01d30ec3046      1
     77 0xb88f61e6fbda83fbfffabe364112137480398018      1
     78 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
     79 0xbb5e3399daf10fbc5094e3f4d3b9aedb7547856c      1
     80 0xbd45c0c21ad360df43ac5c5f84979b988eb6b74e      1
     81 0xbfeff7e87b2bb80fbedb8e35bd728f0cf2c61061      1
     82 0xc02f533a819c4d3149544dd1a55cf7cc87a8d30b      1
     83 0xc2b3cb204e96270d763c70c75ae1a77c739b0e6d      1
     84 0xc76ec56cd15a7fc2ca7d1005710c06ac6e5b28f7      1
     85 0xce1eb9636c412245c92352d6fdfeca9f97a89a4a      1
     86 0xcf0d46e050558a901ed1a7c31365ceb075e2940b      1
     87 0xcfd30ac0732f52f9d87a7ddbeaec96eee76bb5c0      1
     88 0xd00911c7f9e5993ea0cd28cb615c6b21a0101666      1
     89 0xd36d0ace6b18f13760376751af10741336131ce8      1
     90 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
     91 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
     92 0xdb25afdb6b1556a11c5e29aceeddf497a038a09b      1
     93 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
     94 0xe3262eede42a5eb5189b7a57443257d35f7fb5f7      1
     95 0xeb193090edbd341f606c30af743b48943b750272      1
     96 0xec2bf4a144a11a4cf38be451b9db0633bb851230      1
     97 0xee34bbb4882c0ca569f70901217e6aeb3b347b62      1
     98 0xf211a740b547ee0ecf34d48ab7557b7a84b231d1      1
     99 0xf356ffe40599eff010f67f76ed01d4e41aaea276      1
    100 0xfa8016af3abb381723d62f352da0c5ad39c8503a      1

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
