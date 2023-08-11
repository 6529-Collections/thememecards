
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:18682       Length:18682       Min.   :1   Length:18682      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:18682      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17882969 # https://etherscan.io/block/17882969
block_hash <- "0xb07fe44a20368f6acf15bb335de465ce014cffd53797b4646d40e10f39eab392"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4694 

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
      1 0x0594ad5995af0c6107ae1c031eefebe3eae42c1e      1
      2 0x061ae2b316ba7808763be9032b23c0a35a64a2be      1
      3 0x0851ced43aab7bc38b0fa4fbc4e3849634d2ca67      1
      4 0x08a9ead5bd9af49a1f777a9f15929871abd684c3      1
      5 0x0b071354be55d703b46f20e279853ca8f4e28ee4      1
      6 0x0f3ffc89b30f5561d49c11021d5f7164c877c2e4      1
      7 0x107752288b215467de25c16119787d715ec8e26e      1
      8 0x11099ac9cc097d0c9759635b8e16c6a91ecc43da      1
      9 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     10 0x13e8810a25171327c55a2903ff00704f27c0c8a4      1
     11 0x140271fd98e2c9d4338978bd983b3c2e4b20f6f3      1
     12 0x16363a953016b9b941da29a69f361f2b7a28dd40      1
     13 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     14 0x1f2f6361023b414ae8325fbf14b6a502269c346c      1
     15 0x1f8dbbc9445f82a2df48dfc4aa80804231b1f9bb      1
     16 0x1fb95fc1bd13db144c0248cf1b6a0f44ceec93d6      1
     17 0x230f4981634d3c96ee9066f17652dc8b9049015f      1
     18 0x2bc52900a118390ea8a97b29eb5e7e75f1cd912c      1
     19 0x2bde15dfbc4cab612f8288b9e2acd8f6298ac018      1
     20 0x2bf308d324985dd121c366fd3fc398ec19d6651e      1
     21 0x2e8f6f8662593134dc8311e312885921855332bc      1
     22 0x30bc69d36190af10ca920058c5cf7ed8b7233dd4      1
     23 0x31eae19c7737925fad2ff2f007ccef3c9a7e2239      1
     24 0x32dc7a665af94cb941de28de8bd69b8ae95f6b78      1
     25 0x32f72648a8fb1f97c6688e32cd5d5be8a8e25ae3      1
     26 0x3607eda07116b277c1f791ee224cb2ad9291a009      1
     27 0x3c99c5157416d57db4ac590246bbf528211cf501      1
     28 0x3d055040d91414fbdc0a9eb0cb4e1bdf222fb1e1      1
     29 0x3f7a28ada099123ce1dc8e695f1ead762a239c62      1
     30 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     31 0x48116616e768cb2ccf933bce172dbce9f6d9a4ed      1
     32 0x5391ff3a579bbcb8372638f62defc7773a8f55e5      1
     33 0x56879fdf9792ae08e890528815a317ae53bb6ddb      1
     34 0x56cd7d3e1d4d25d826a95a0c1f5d172e29fea651      1
     35 0x5712ec69b1dedf934303ce530ae2b4f1d3ca4c61      1
     36 0x587d5b38d1ccfe08fa59230c1d5bbdd8e66e4ab1      1
     37 0x5b60608b5a9930114a3587a97bc93b5091927380      1
     38 0x5bf4ed7f13cd6441834aa0a1ad65d0b79d478951      1
     39 0x5c0ac43fa40cdbfe6d86e9041052b2eabb799227      1
     40 0x5d2035ebe68f4ce03d6822a68472d82c74af18cf      1
     41 0x5e07e40466ccff3baeaf370ad36fce57bbdac247      1
     42 0x5e9c0665630b8659180da179cd789edea40152d3      1
     43 0x6083b3f26dc6d5a506f71958f9cfc363711481b4      1
     44 0x60d4d538ff67e6bd2bd14d3aa724014ef01abb93      1
     45 0x660c3bf8d0f8b84a9e1e7a076cfe4685128f5f7c      1
     46 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
     47 0x69471066140252f1a7529b71b56cd06b6f390eb2      1
     48 0x6dc43be93a8b5fd37dc16f24872babc6da5e5e3e      1
     49 0x720bf57f67b6b00b97cbd9d967a0af2427352435      1
     50 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     51 0x782adafbf47a604f146af4a059908e946eae539f      1
     52 0x7cf91c0ef074f4e3fbeebda55dde44ddbd20443e      1
     53 0x7deacee45d121a35d3c8db640ee7f9b437b4b2c8      1
     54 0x81ad1a19187f0f3444c0f8bef98ea49c1b9fbc03      1
     55 0x843e5a575fa950056a890861df920f7f8ebfd4e7      1
     56 0x869ce3c7415f06db4ff6d599d629c1a9c7c8a820      1
     57 0x86cc7ad354441bae1304f55c1f6df8ba98d44af2      1
     58 0x8753982d800bbbf1faf478ef84c3e96f2775d3b9      1
     59 0x87b1702a5a31f6b63820c99f2234a3323ac728c4      1
     60 0x8d9d8d19e396dda0250eb9b0eb8dd631e612190f      1
     61 0x8dd62416fa3c6efada57eb7a6383885045390204      1
     62 0x9429b7d2a312b140e18981757b2b44b33881e76b      1
     63 0x944f72d4bbfb493090c8d17ef19bfe49ff26bdf0      1
     64 0x94fb7f46728d4030e40b47396eba626fe2e5df50      1
     65 0x99a5331b9aacaef3516db30d9c59efa6c760087f      1
     66 0x9a55ccf605ff9d08c255d766b2dfbead74840053      1
     67 0xa17bfb3a816996ac3a1e26dddcc7850663548c16      1
     68 0xa22414e4af5767448624017c7e0151d22490412b      1
     69 0xa26979cacb7da6b0218f288ac13e88c07a658323      1
     70 0xa7c3f481810149f3853993abc4e85c49023cd2b6      1
     71 0xaefd26382d84a5f40403a8787b51290908ac9cc0      1
     72 0xb128b2b054a0d57a0dc3e6cefbc65573cbc29f74      1
     73 0xb20898ad9ae01fa34fb6746de24dbd599c886e22      1
     74 0xb235c9fc67e6d850a31035432bc2d200e350743d      1
     75 0xb406ebb1a43cdc9c6eb5b392c712329c0b84d546      1
     76 0xb49806cf8dcdb108d318d6f4f7ab087851445d94      1
     77 0xb509189877c46f92cb784d59e1fb6dfc03cd7ede      1
     78 0xb6037b6feb9406942649885959346755b3ad04c8      1
     79 0xb70c3049982c09c18dcbf8596ccef6f5b3c239a3      1
     80 0xba83d8ef99b935b55deae87e9623b4c5d5baef88      1
     81 0xba8db35dae41465a45ab494a5a841c73ed1c047e      1
     82 0xbd9b7373aac15d9a93c810df3999343f4fe1ed88      1
     83 0xbe58a1b85d11e2083bb7c5766284ed7848448d1d      1
     84 0xc0f628e4b48ffdf40d899a4815bfea125da976b6      1
     85 0xc29b6d6a91f5cb8efecb4722d4150ecca925434f      1
     86 0xcbaddb16544a736b48b455812d28ee71a54ad6c0      1
     87 0xcda2efb1de50aa4476b4a20c36bfffdf97b5ae80      1
     88 0xd1f3a51bde35942789eb3b3ba205fcdc340c8552      1
     89 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
     90 0xdc7bfc3058654da90692457f68ee3a34e2dd3908      1
     91 0xe036a80f628e531982189fb6c4f277deca521e36      1
     92 0xe1d8963bead06af2f756542b5bb673751a39be5b      1
     93 0xe22c2f9460e76daf26411d46911b66232f4f1253      1
     94 0xe513e29ff7ede7e01eb96e2d3fbe1b1189386d06      1
     95 0xe8eb234375d59df64823ffda70207e26334ceeb5      1
     96 0xeb6ee7076521f9ef9da69705426d2335f27c5168      1
     97 0xf0f64ff70b6d85cc2cdbf1bc362e930b9ff519ba      1
     98 0xf58c03997171ec5e5ccaaba7338d0575d5616823      1
     99 0xf58d6a3178b34d10a0dd1bbfc7860d0f58fa4d0d      1
    100 0xf897c1939b330e3bcaa8ea8b3637527df5accb4d      1

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
