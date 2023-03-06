
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:27383       Length:27383       Min.   : 1.000   Length:27383      
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.015                     
                                           3rd Qu.: 1.000                     
                                           Max.   :19.000                     
         name          
     Length:27383      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16761269 # https://etherscan.io/block/16761269
block_hash <- "0xe3b7ca157dd44d9e988fdc20a5b69d836817f693f5577330cd2ea79ba856d86d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4695 

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
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

airdrop_memes_all   <- pick(snapshot, contracts=c("memes_full_bothSZNs"),address_remove=address_remove, address_pick=12,address_max=1)
airdrop_memes_szn2  <- pick(snapshot, contracts=c("memes_full_SZN2"),address_remove=address_remove,address_subtract=airdrop_memes_all,address_pick=9,address_max=1)
airdrop_gradient    <- pick(snapshot, contracts=c("gradient"),address_pick=9, address_max=1)
airdrop_tokyoluv    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","Foundation","HYPERREALITY","ShapesofTokyo","TokyoluvCollection"), address_remove=address_remove,address_pick=30,address_max=1)

allow_tokyoluv_singles  <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","Foundation","HYPERREALITY","ShapesofTokyo","TokyoluvCollection","NeonNoirTokyo"), address_remove=address_remove, address_subtract=airdrop_tokyoluv,address_max=1)
allow_tokyoluv_editions <- pick(snapshot, contracts=c("NeonNoirTokyo","TokyoluvCollectionEditions","FishyEditions","CapsuleEditions","TOKYOLUVEDITIONS","DokiDokiEditions","Decal"), address_remove=address_remove, address_subtract = c(airdrop_tokyoluv,allow_tokyoluv_singles),address_pick=400,address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_subtract=airdrop_gradient,address_max=1)
allow_memes_1_phase1    <- pick(snapshot, contracts=c("memes_full_bothSZNs"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2),address_max=1)
allow_memes_2_phase1    <- pick(snapshot, contracts=c("memes_full_SZN2"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1),address_max=1)
allow_memes_3_phase1    <- pick(snapshot, contracts=c("memes_top_500"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1,allow_memes_2_phase1),address_pick=100,address_max=1)
allow_memes_4_phase1    <- pick(snapshot, contracts=c("memes_random_50"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1,allow_memes_2_phase1,allow_memes_3_phase1),address_pick=50,address_max=1)

allow_tokyoluv_phase2   <- pick(snapshot, contracts=c("TokyoluvCollectionEditions","FishyEditions","CapsuleEditions","TOKYOLUVEDITIONS","DokiDokiEditions","Decal"), address_remove=address_remove, address_subtract = c(airdrop_tokyoluv,allow_tokyoluv_singles,allow_tokyoluv_editions),address_max=1)
allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_memes_phase2      <- pick(snapshot, contracts=c("memes_top_500"), address_remove=address_remove, address_subtract = c(airdrop_memes_all, airdrop_memes_szn2,allow_memes_1_phase1,allow_memes_2_phase1,allow_memes_3_phase1,allow_memes_4_phase1),address_max=1)
```

## Airdrop Memes Both SZNs

``` r
c(airdrop_memes_all) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes_12_both_szns.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 12 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     2 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     3 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
     4 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
     5 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
     6 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
     7 0x54913cc8ea17731d62589039dd0152f306473843      1
     8 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
     9 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    10 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    11 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    12 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1

## Airdrop Memes SZN2

``` r
c(airdrop_memes_szn2) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes_9_szn2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 9 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
    2 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
    3 0x220da3fc3b905c968dfb20c81c170bc4dce56603      1
    4 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
    5 0x526ef837f56eb025908ba91811a6ed0a3b32c560      1
    6 0x69e68074f1aada957edd39c5eae0069973343f30      1
    7 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    8 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    9 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1

## Airdrop Gradients

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 9 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
    2 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    3 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    4 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    5 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    6 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    7 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    8 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    9 0xf8aace471723dba6514f6c6d43651611a0c34529      1

## Airdrop Artist

``` r
c(airdrop_tokyoluv) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x066ad9510d992444a856731d6af0888c2b2e5d76      1
     2 0x19142e3df9e34966c876024dfb9d805f4ea4a131      1
     3 0x1a7861a39e74d93c8eb883d774f168be46f81228      1
     4 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     5 0x37729c5948bb726a505b9633caab02521c67d89a      1
     6 0x3da11211f42c3db912bb9fecc0c843276ca673d6      1
     7 0x416eba0df5f99cd0c98226d883bc094a1f934c97      1
     8 0x443037c611e0236c0c3d1e811c7b785d1d360ce2      1
     9 0x55834cd1f57c4a73d643caa176ec8db4da0669a4      1
    10 0x5688eedc08e9e7301c1d7102c95112253b393e1e      1
    11 0x61cb53ba546c5264cbe9324843a25882cb82e1db      1
    12 0x843ecd1063eb7d6aff98e40a104cf3186bca8c2f      1
    13 0x8507a82600e3f2c85459c27721184698f18359fb      1
    14 0x8888888888e9997e64793849389a8faf5e8e547c      1
    15 0x8d1919f9f69911b0c6630703f1ab5da3d3faf1ca      1
    16 0x9fc7962ad74f3b42d78f3f46b4f82b025b91d059      1
    17 0xa23cb68780be74b254a5f7210ec6cf1c76289953      1
    18 0xa7f66d3ff024e639191c89170463b6ac4ff1478d      1
    19 0xbb315ba16c951cdccbab7be11b935387d82d98cc      1
    20 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
    21 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    22 0xc4f370c6d3164a56971692362a9e488c0992a29d      1
    23 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    24 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    25 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    26 0xdee0075b89869cc40135468e861dec990bb4ad6e      1
    27 0xe428d30c12b2b976d3681a17a9db4a185b17db2c      1
    28 0xeb1c22baacafac7836f20f684c946228401ff01c      1
    29 0xf305f90b19cf66fc2d038f92a26440b66cf858f6      1
    30 0xf6d45c42445391b350be3cad097d85a0fe438174      1

## Allow Artist 1/1s Phase 1

``` r
c(allow_tokyoluv_singles) %>%
tally() %T>%
readr::write_csv(file="allow_artist_singles_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 40 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00227878f54356410004d186b2f1d09deccac42d      1
     2 0x04c1c15a1554891fb2dd7c4939e7d95d06ee253f      1
     3 0x076d147ded7414e90b7b629000c392ff7baf8b34      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x2734a7f407d296311a0fd83e04c05e0cc76b4a34      1
     6 0x371c4cf0efeda222fd6461cd5e4f9d62ac61d78e      1
     7 0x385bc89d9f08ed280f676c1b6a9285ece386083f      1
     8 0x39939486609d55a3424ccdd22fb53bc209321418      1
     9 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    10 0x442a653a06482079199a106079240ee54ceaf890      1
    11 0x47f20a14d7df028124c631348c69681b1e8b5bb9      1
    12 0x49e2a5af17fb2b37a0ff460217dc7a9ff4f34f90      1
    13 0x4d5bf3886935d0966479c651a3448ffaaf32252b      1
    14 0x4e9cca41646b60a6817a984c8721d8e7e783c7cc      1
    15 0x5b93ff82faaf241c15997ea3975419dddd8362c5      1
    16 0x6277523bbc2f82612522725cda91db871de75c81      1
    17 0x6a6743b85899cc2571c8034e209f74f5a92a0e9b      1
    18 0x6e2839114cf8dd8af13b5d5e3bff88f832fa3548      1
    19 0x7843cd3d61bee45530b1e6957cdd4aa5fe539207      1
    20 0x8497277c9339170a0420e86da8352e0c084624cd      1
    21 0x86ac4124cf47e2ce3c41353cf8072735b8db6184      1
    22 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    23 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
    24 0x9e061e73f28b3793183febfe06e544fbf61f5650      1
    25 0x9fae8562dc4fbcdd9d26d6d10a7fb88ce60a028e      1
    26 0xa601e370a596a261ae7071d639e7ea11148a201f      1
    27 0xb71e439163bd943f52a312146682a2925adc9044      1
    28 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    29 0xbaed0b390801f916c76b4a6a41223be71669474b      1
    30 0xc62391af720c1a2e5d8376a6c103ddf8e2a7180c      1
    31 0xcbee390e62853b80aaae7181b205f6cf368cff7d      1
    32 0xcd0b5a7c77d1eccdeac505b871014ad59bfe6673      1
    33 0xd2e47ead288603aa9e33a8705bf68ab17dc5ae1f      1
    34 0xd532962fd7976880fdff92db9cbe48a7369b1fc0      1
    35 0xd805e0c7822099d37b871d602a0a1c7ddebfda4d      1
    36 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    37 0xde70bdf7dfe13614d281716813274380a59e3e5d      1
    38 0xef70b950aecd42aa586fdc6141f2e25f44cf74b4      1
    39 0xf5556f61810e6c8c0741fd03f20032af4276ccdb      1
    40 0xfefc410741c00f4b80c69367656d56bc6ccf89cb      1

## Allow Artist Editions Phase 1

``` r
c(allow_tokyoluv_editions) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 400 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0062e55a9d5e5972ccc5ad8890a930bfd271fbc9      1
      2 0x008f579d880e7b036f21bebf3918f1cf25347a42      1
      3 0x0258558bf2a4ffceec4a2311b36ef124d3a4116e      1
      4 0x02f399ef29e2f8e52fe700e321d82be6966c7ab8      1
      5 0x047d7e0a147026e41169f0f168986caafaa0c6c7      1
      6 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      7 0x053e6294400a9268e35df445624f58087c7f388f      1
      8 0x07cd20e5c476d7b3bcc4a0bd88b324d182f8670e      1
      9 0x0947f088c7486f9c1f022e52793dd57e0dcf4ccf      1
     10 0x09a31e9ea6490991995d4ecec3c5748b993064fd      1
     11 0x09e9248a013c0a787241bb0bfb3403954a96590d      1
     12 0x0a00f2a39633e4106ad37cc4c4e10c7f30d77c23      1
     13 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
     14 0x0cb07c4d102ef9e05bc70983be01174ddf332c2b      1
     15 0x0cb9e54cac1f43fdacb6522279cde589913b9786      1
     16 0x0ce390f18af702cca546297845a4a51d102123cf      1
     17 0x0cf6ef3e4053f1e5f69584037fc77f550c53fe2e      1
     18 0x0da5b59f9439efd8250c1055332c818d01a59ea0      1
     19 0x0dc903afb13b941e2967f4cb1c37bbc91f5ccaa7      1
     20 0x0eb8a744687e9795db536240254bf5d84a695acb      1
     21 0x0f045a0d77d24c326316e0315354e7df28b4ac50      1
     22 0x100742ac3c801564932c088b060b5613bf6e1e70      1
     23 0x1064aa646a7aedbd40816fc0c35e044d0244a764      1
     24 0x10f97e62ae2b7be0ff2ce8e09d05688734f2eb41      1
     25 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     26 0x1200eb4fa3df9903fc6eff1d7a4a5d17502329b2      1
     27 0x1261e144cd43d6db5f0ea4f51d6456ae39e46d71      1
     28 0x1287b81d7667a1721e8ffc7463b86ba78335e295      1
     29 0x135358d6701b1c98ccfcf2ec45d9643ccea569fc      1
     30 0x1418ab54752826a82698a23e22ebbf7e678cfe47      1
     31 0x145358fac8cf55a4d14548169c8424f7c0f03c95      1
     32 0x14929281cb3dc7f7e47c95a977c05ed4b85489d0      1
     33 0x15008b9e6e5b8bb3dd270d28c13bba209cad664b      1
     34 0x152d1819d87e3739fa0ae17ce4e468ea7dc0db86      1
     35 0x1593c9ab842c36a926ae39cb2173731f9362e8fd      1
     36 0x18c27079a7b69e67b660bf9839ba2b652cc3cad0      1
     37 0x19066dbfe5ce63f6b80d0f8b7b4f0453ce947eaa      1
     38 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     39 0x193ac8ecb86c04292ed3f2afa04faa4f384a0dae      1
     40 0x195506d922d359bc4f6ed052712ef1383033af09      1
     41 0x19e205de68a48bd872fc0caac877de27c1b37d3a      1
     42 0x1a6a6e6f31fc110b011f9afdda4aaa02ad5581af      1
     43 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     44 0x1a71f91a8cdd2b36bac8e5c29589660fa00db8df      1
     45 0x1a79f73b8209aa881ea2f6ac1ed4a15e822b1677      1
     46 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     47 0x1c73c6174eb2fddc2e0a702e85cd3146c05aa122      1
     48 0x1d932e6489b5e4091ee078db497799237b354191      1
     49 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     50 0x1dfa0ddfc2e523736d98ca7ca3c35d5ce0b2cc70      1
     51 0x1e5d3b3b2fc1903e3202338a206ad587b54f77cb      1
     52 0x1efc22a134a410fed4439c1b11b010dbeb8bd5d3      1
     53 0x1f687ff00fa6b0ce7825e9f90ee4f05b5b8ecee6      1
     54 0x1fb285a8efd16dd34c608bde845f521befaf605f      1
     55 0x1fc46d86890cb931432ba21e0fa05c70ff9b0ff0      1
     56 0x2153cf39bac90b91b6d9f014012ac89424f9c34e      1
     57 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     58 0x223a35433f3f87ef302fdfccd77777dd0cd51f80      1
     59 0x2277ba6046c5d50a85544e68332fbd359a24bec4      1
     60 0x227f9567667f864a9399afb892e83203975b7a2e      1
     61 0x2284fdeea9c1d90d9f47fae7e6a7115500360d4b      1
     62 0x228bef585724a558de9bc4b8e88702c9be2deb04      1
     63 0x243ae63fed8680067e16f63546d312aac1f5d716      1
     64 0x24519bb072e7303fe40793a7627cd1dd08eaeecc      1
     65 0x24757d9aea2086258583561141e4ec036bc28a20      1
     66 0x24ba13e030757aec2af3e37e8ebf150f2bff79fc      1
     67 0x251a0be0e10f5dce6908ed3ec3777fc71cc5b420      1
     68 0x2745bd23d9b23edf1ff4b695729c4804c111676c      1
     69 0x27eb78c1eade6fc040d25b94e7acf6bbe0689f0a      1
     70 0x284003fda3d98e3d8698085f5901b9bd2d1c7b38      1
     71 0x28c756268ca3bcc488db5554578dc4ef74bb580d      1
     72 0x28d209fac8dac60742bd2f1945bd46fa88d708b2      1
     73 0x2a128e3baa2ccea99c8b066bd7bbbbeb3eb37347      1
     74 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
     75 0x2ab5267020f47fc6311bb296d9e06ac43a49c81a      1
     76 0x2c2ea084c3c4ce6698bc211c0dcc9ab533a18c2a      1
     77 0x2cff2f3127ef18432109aacd32769053fde58d9a      1
     78 0x2d1d925f7144a3dd56913eb4bbd011b52dc2a2f3      1
     79 0x2d3e559693e21da3d0c150ac580b5ff81c46c9da      1
     80 0x2d8048317d31c4d58e54b55a38b21c45ab7a6507      1
     81 0x2ee5bed2575f9ce0f777eec4b8e7d31d5c854380      1
     82 0x311f2b05f8c9dacf4443da9951443664368e726b      1
     83 0x312e7263e9b7596ccc38d25fb847db08c74e5564      1
     84 0x3168a9334c9106bcc947b4ac0818eb1613b91e64      1
     85 0x31a6d0ea27db941257024189a3718472d40ef663      1
     86 0x32503329b82322db6e491975890dab19eff8dddf      1
     87 0x328809a567b87b6123462c3062e8438bbb75c1c5      1
     88 0x32e146e660e74f2a06e40f7b65cebdec0856b2f5      1
     89 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     90 0x370f2b7cb212617ef1353bb20e8e66dc5950374f      1
     91 0x39183cd265f50af7533ec058a4c807ca9bb6d10a      1
     92 0x3935e58e11d301dc1d7b09c58e2b1b28f08049dc      1
     93 0x398017f0a1a3f3e581f2f91578549f20a3eaf6a2      1
     94 0x3a017854138c5f9b85b0457b832151b28213b6e6      1
     95 0x3a94ec43996178f7013988358f58baf18a4cc707      1
     96 0x3a9719d24000dda5755a294ef3c08ec8b526908c      1
     97 0x3b31beacc45ae9228c2b97b07b3850c71f2dd7be      1
     98 0x3d66a8bece916eb9cfbed8d944eccae0ef57b0ae      1
     99 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    100 0x3e427083532eaf609bdff0f85fd9ff6cac2e81de      1
    101 0x3f426a31a77afd8c2590146b8994755b0fc71349      1
    102 0x3fe83c379b5c96bc31ee3cc4f858a8335f7572e8      1
    103 0x3ff817113ce8794b650f676bfc89d90fb699362d      1
    104 0x406e83146b1c0239bb7a271d7719661d89871f6b      1
    105 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    106 0x40f465f1ba4c2aba91c0c896cb92bbe4c7e545df      1
    107 0x4166b70be9ef1f06eb562e6aefa32d4650b716a8      1
    108 0x4207b5cc0664457f6c08a74fef55c21cee181cb9      1
    109 0x426ac0453bb3d96f2a76c5634e92d9f19761f8e0      1
    110 0x430081ae98a756e8941cd01534c1a91f32825ccc      1
    111 0x4310c23b91652e725de106ce2ba8c785237ec772      1
    112 0x43f6804e203a6e96ace054784973073daea68246      1
    113 0x440758f4664c99d00ce72bc452b40dce8b7023b4      1
    114 0x44ff5b306885d652fd46efd512678d0a18120b42      1
    115 0x46645a800dc67aa6a4e42fa7f608360a587f82e7      1
    116 0x46d7d720e2968ed2e894d491ea3fe50e40c85450      1
    117 0x472c6474885cd085b8cd1c5fe23f680d70d562c0      1
    118 0x478bb98378fa40c1cbc8abfdfe6029929ff3f854      1
    119 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    120 0x48ddb5e74817105a6911cd9a747fe53411e8d601      1
    121 0x49df36a77f819190a41368bc8aca8a54fb6d760d      1
    122 0x4a1a0aeea2a03ed451b577f51f1d7e5568f29736      1
    123 0x4b933cd816c86783b116d070818c6fcdf5bff17a      1
    124 0x4ba3f93ae0780c50db799fbe9012416b4fd797bf      1
    125 0x4bfde9c1ab8887452a2a9fb80b6f60e013108ea2      1
    126 0x4cd8e0bd4a991b2d7763d80c4c63dde5582942a5      1
    127 0x4d5a7716d0c4eedb7f6b64f89ab7ab383c94d9b9      1
    128 0x4dfc2c46b01698ff74768b89f4fbfa89d8ee09a7      1
    129 0x4e6180f2eca3f9bbba840f324502a95cf38cb468      1
    130 0x4ee589b12f56aaa3c4eaa4dc7ebe0b62b7c02cd4      1
    131 0x4f4538e2553c61bce747f73c5fe8133d4b383dda      1
    132 0x4f984cc05d35d90c76bd1a3d4041a1922a164d5c      1
    133 0x4fc1252351e19e9a2c0204d818b2371c23492c12      1
    134 0x50664ede715e131f584d3e7eaabd7818bb20a068      1
    135 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
    136 0x52e284276f9b0d5cdd33f71ba11448f9126eafb4      1
    137 0x549c262d21164b57376704813875d5bc558e5dab      1
    138 0x54eab4dfd0773d95ff0661f80079beab81f27399      1
    139 0x5655a4ed1ac0928bad65cceedcc8e5012b3d0575      1
    140 0x56ecf119d56b3c6c1b55128cec8c38c70d3cfffd      1
    141 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
    142 0x574a8af005c82eaf05e92c9ccc01048e1eb1ae41      1
    143 0x576da0260daec56d8ced30536fe9fc84e7d15542      1
    144 0x57867bcb9d658abade41dd55a9bbb38149e6b9ed      1
    145 0x57af2c4249761f4b266dbd80fe44061faf4780c3      1
    146 0x58632e46c01732e36bb386e6c02fe49d5310a4b7      1
    147 0x58addb0456871d27fd96d58c9efa0ad4d90d38b8      1
    148 0x5ae1945797d10c71a53638c6d9fe8c1a36224853      1
    149 0x5b33b6f41e87111b7d22685b297f086b5535034d      1
    150 0x5dfc1f36bee4938b3dc8ebdb4c34f64c935fc2f9      1
    151 0x5e11534344b8c1fda947b37dc57b8734232a6b1c      1
    152 0x5e53eeabc71338d8c541434ccd7e6b2ebb04dd06      1
    153 0x5f20d84dbfce7d9bd5b43dd22e650e1ef017c011      1
    154 0x5f2abc70bb1567c7807da87e944ddf965a0846bd      1
    155 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
    156 0x605aa36136b4fb935b57b0b842de35a967b8a4e1      1
    157 0x60c3bc61d0432982105ba248a61da006c1bf2d8d      1
    158 0x6177c71d568960d87e958c38000350ed3276796b      1
    159 0x61b50a74376a9cd1dabf20bc922017441f9f8267      1
    160 0x6234bdb734d333194255bdcb3d26eae333cc1446      1
    161 0x6263b9edc7a9ee22789ec45995d17d9d21b8a609      1
    162 0x64072ae2095677ab7e8df63e1fbae89b6366d6b4      1
    163 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    164 0x66c3959b636bea1a86e3133de6c23df342c5c4c3      1
    165 0x66ea2d335291a5afa8d3522eef6adfc3f8746e16      1
    166 0x6742d2b4aa4df34bb9e3fbab8fb206e1e92130e0      1
    167 0x67744f4b07a3708da6a2362739cc0872e81a6555      1
    168 0x683c3ac15e4e024e1509505b9a8f3f7b1a1cff1e      1
    169 0x6882185ef840a543643d9cdb8f41d1c6b0bdeab7      1
    170 0x6882f12587dce566c3cfd165dcf7dc6bc35812f4      1
    171 0x6898c66223313e5327addafa72a33728f8188ee6      1
    172 0x68dc0f7ba4efc952b23e6cacfdf11de87b54a909      1
    173 0x690f266d5349f23c2131ed306b4f28e2d8776972      1
    174 0x692d1fce318f98865203a256b1024e070295c6f4      1
    175 0x694e6f104997d48a56c40e0390feb3e6bdfaf717      1
    176 0x6b281eeabaf6bed8ef9e377ca7e1e2b2469ea2ff      1
    177 0x6c18430fd0b4bd420c9aba2af79f240d522051be      1
    178 0x6c3d6d6038b40854b811b4dc8310c04aa68310c9      1
    179 0x6cda6532ec8a1b880dda5b8cf0405ad86aee4e51      1
    180 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    181 0x6e0c83fba7a67b7c64bfd1ff7760cbb096a5da35      1
    182 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    183 0x6eb27492cdf18a67684402d159313d613648d697      1
    184 0x6ef8fd2ffd39df9e73eea701e17db9cf51ffbb7e      1
    185 0x6f2148034c9615c0dc0d06589caf73d526d000e4      1
    186 0x70f4a2ce11a2524b6686963a5de86e336ee56ebb      1
    187 0x71784687d4c74338bf284bea22956c74fbe6d631      1
    188 0x71add64adbb965bafd01437295968502eb61145e      1
    189 0x7291d089d9dbb74b156f08910b1741c26665fe8e      1
    190 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
    191 0x73147f1a2ebcf284b2d0061299bda8608fe0177f      1
    192 0x735aa397cd3bb90350ba40fa5838e109581c3dcf      1
    193 0x75821898ad35392425896085089629611391d2bf      1
    194 0x765d8e61bd060d0400bbe2a1d648b225434ec9f2      1
    195 0x767e04b2421a4f995aa51cb34d4a48e64053b9ab      1
    196 0x77424437e320fc70ab04d983e259ca6e6e205c86      1
    197 0x777c7f8334640dc41e1da6478b90f6c00dcec724      1
    198 0x785fc9d0cdcdbc6da73257c2df634ce962f6c7d2      1
    199 0x78b611cc67827c0dcbd326b65fb12c94b73b966f      1
    200 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    201 0x794054f4847d116c2f553fdcd4a2f1ae0a654da9      1
    202 0x79c1134a1dfdf7e0d58e58cafc84a514991372e6      1
    203 0x79dc911ba6f8424a4b6158ea0b24168a8e0e0fe4      1
    204 0x7c0ca862b604c61a7b7e78647ad95563f60956c2      1
    205 0x7c8f4a31bfa6a2bc70e538dec4636da3c531abe8      1
    206 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
    207 0x7ccb7108e4ca5cfab11592005656d820b91191e1      1
    208 0x7cff4eca2bc08ddd3b6b2ac597d9bb7eb4a2e341      1
    209 0x7d0cbc828104548265082e7537a9c0d3c4a6bc9f      1
    210 0x7d989ce97cff246e23b2c8e9d431e922c3e85dea      1
    211 0x7e698afd992dd40bc466dd072ff9bc272d3bab5d      1
    212 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    213 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    214 0x834657f945e839837aa6387fcbffebb104aaf2d4      1
    215 0x8362f413b900d0cb0f379d3a55ba454244348b22      1
    216 0x8369b9defc618e5c7d9649ae523bbe92eea0e39a      1
    217 0x843793006d45402566fc232dde7050f1bd892c73      1
    218 0x8485867178fde14abad7ebfaedac46e50fa41288      1
    219 0x85a494506eccfb7f41ece80e62335fa1d126fe4a      1
    220 0x85abbc31fdd045c38d08c500aa0a58d40ab26cd2      1
    221 0x863c24bd7432ed0498092734467dc8594a6a0533      1
    222 0x87bb150e07ca04ca038cfe70c3c67ef4fdab3a48      1
    223 0x87c334d53775cb5eb8c15326e54621443da5f51c      1
    224 0x885846850aabf20d8f8e051f400354d94a32ff55      1
    225 0x8868e984009e484a37ee63ddf1bea2531a099aaf      1
    226 0x88b07f430375d6784e5cf062b596043f76b38d7a      1
    227 0x88d5eb1993dd04bf2175f940e64fd49a90d13f8b      1
    228 0x89abb2d3be7bd322275033031658838735b6961e      1
    229 0x8b54fff99b9b6a35b5bbe07424b14caeba02cf8e      1
    230 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    231 0x8c2df6fa5af281f97a041d5c0429f9c249fbccad      1
    232 0x8d3edf0b0abdacb72525c8ec4c7ad607ce5f6949      1
    233 0x8e611b4f46a353aae8fa55af10b5cec24f5f1db8      1
    234 0x8f9be4310f9abb0e5843cc6363908c9b01dfeb3f      1
    235 0x90a8734076d7a7db4303fed64cf46467069f11d8      1
    236 0x90c1191003ffe2baa459d06775514aab04c0b33e      1
    237 0x91d4b65945c3d9a315103fd5314bc62caf8b29b9      1
    238 0x9243391eb953d25228d5845f7514952edcf12958      1
    239 0x92730185034d49ce10f0b0670f2ee3857415f178      1
    240 0x927c2f09ddc5aaca143164e8cc0c774ebc71b0ac      1
    241 0x92a01034023e823b0aa661145443bcdbeffa2cad      1
    242 0x9311b27b11360ffc02f1577141f71c5042cec418      1
    243 0x93a5e5a05cb13eefa92cfc15a409aa316b473710      1
    244 0x93ac750ed2bc4332bbbda8d60a428296b868dc95      1
    245 0x940acd9375b46ec2fa7c0e8aad9d7241fb01e205      1
    246 0x941beced3e87a15ba22e1a3705b547f50cfd2eb1      1
    247 0x94803d6c3cb805a308b9814202d7d01dcb26049d      1
    248 0x950ae67f79bbddc8264e822412be872e0fac56f8      1
    249 0x950e14baf6c4074e1d0153055f5a3cc3a33c4365      1
    250 0x95d2d5835caac8ae712e5357ff6f10857c431d22      1
    251 0x960f355ad4e8bff1b6a3b8d64faa4b979775fa35      1
    252 0x97e7f9f6987d3b06e702642459f7c4097914ea87      1
    253 0x984700290a73d76578c56fa78e66e15591753c3e      1
    254 0x999f5b05702532fefa42e28e260e54a09ebbb287      1
    255 0x9bc38bf1d3f1161f831e45ae8295090eaae8bfd9      1
    256 0x9ce6840743f3d01550aabac539056ee7258c13ca      1
    257 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
    258 0x9dcb406e71a365272d7caea9723fd8441fbb1471      1
    259 0x9e68003214bb9a47336039356a54ebbdbb1bd591      1
    260 0x9eec3976435a37b0340ecbd966c226a691956b35      1
    261 0x9fc4001133d3825b7ce4b05bcfbe227fb3cd5fdf      1
    262 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    263 0xa086767554481c46da8840d84b05e6dfd7d7364d      1
    264 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    265 0xa11d442e1cf701a27431f695ddb95e21288cc7b3      1
    266 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    267 0xa405f5729e76d303c0f0cb0ac9b8200d9c5b85fd      1
    268 0xa4ae4aa7b3c91bfbaa54c7e96be9e7810105f79f      1
    269 0xa5f274e7ec2bfd3ab37b411ca0262572386ba1ce      1
    270 0xa61186ef4cc522028546cb79761c4c8dd65c6c3b      1
    271 0xa79db9845d9ec3b40681594e3e8fbe72778f1ea1      1
    272 0xa7e458a1b32070387e7548063e1f5e7f3982e6d1      1
    273 0xa8674b0f20d44fe47a3f733036038977cc31f8fd      1
    274 0xa8c33f4621d320364e851f4a951d2722e29bba86      1
    275 0xa93450b6e8300071e72a5da104c7bd5ed4e102c5      1
    276 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    277 0xa9dc8eea3af50e1a3059c09f6bf4451cc9ceb3e6      1
    278 0xa9ef46b6c863b7d81b6f1bcd4cfe9e50275aa197      1
    279 0xaa2899ac0064fc11e95a0fd5b4e05e3432d89d71      1
    280 0xaad06a4385f4797c69633bd2d16e0bd31dd7232e      1
    281 0xab122311c01859354f85e0d87d054c2d9a3c1801      1
    282 0xab45160c38e9781347fad728b7331e2601812198      1
    283 0xac379456191ec7a658337c278b13d22b3ee2062e      1
    284 0xadd55a5acc7ef3a09789d7cbb059871a17d32391      1
    285 0xae59bb14caeb87bd77e609dbf62de3c6224ebc3a      1
    286 0xaf4c0da9c2d9018dcc55b8cbfb96fb9348b61765      1
    287 0xb023615ba92d0f2a6736c17e9c6c3b7cae0b1627      1
    288 0xb0574ee952f53f65e738f2370a70625f986eefb6      1
    289 0xb13d5bf341c2bffca35bf6dacce8cfc19402996d      1
    290 0xb18af710a4b856c6a5a6f13b04259fe8e180213c      1
    291 0xb288090e6b2427e2e4b52afb0590f0f22ea0f6d4      1
    292 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
    293 0xb32b4350c25141e779d392c1dbe857b62b60b4c9      1
    294 0xb35e4e46f6bb4a0701b7afe137dc4a06170681fc      1
    295 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    296 0xb83216ce43714fdac527d809dfcd9dd07d11623a      1
    297 0xb8de1da89da008eb7bb76ab53c6e1db5d3f72967      1
    298 0xbb246cdf518a82c454aa0bf5f914446b7d94aab4      1
    299 0xbda838ef1c38716808e6e8ea6e71cd7c8d0ac8bc      1
    300 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
    301 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    302 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    303 0xc0ea42864f9086c233025c714ac0b0dc9adbe8df      1
    304 0xc15add7eb1ba708bc7189cef6a4c47200b77a52b      1
    305 0xc1839cea93bdfdf29e08ae3f351813fe7e89afb3      1
    306 0xc28d1f5a6d545930a0b0468d8bf388ef866b6dde      1
    307 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
    308 0xc34aa5ddd90a6af4d89ebe4d0026f439bf11368d      1
    309 0xc3d5d087795e0ba0a8b9ca574299655bed83161a      1
    310 0xc403d1b4a9ca1da1abfade5b43211566820dc494      1
    311 0xc569ddad0ba0f22fce53b0422a0f80acc9f8b1d3      1
    312 0xc58ad3fd304cc127057aee9b4bcc79fa133ac59e      1
    313 0xc5ad577d057676a6b4853280112fa399fb1370a5      1
    314 0xc648e78e7fba47717fa93dc27053b99d9899fe4c      1
    315 0xc6799cdd94b61660ad757f536b2b3c69a374bf43      1
    316 0xc6b89634f0afb34b59c05a0b7cd132141778addd      1
    317 0xc79d31c1f25dbbe3b79287ecd273b505d9b1b3d1      1
    318 0xc8dd545b6076ae732e97ec0ad6a3fbf9518db2f5      1
    319 0xc98b814665769c2cc0bd4907b161ad3dc7cdbb2a      1
    320 0xc9976abb6f26484c1c6f9220d8387f71dfc45821      1
    321 0xca33ea12e2a6bb8e5bdde2a102bc61d78760c96e      1
    322 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    323 0xcb3408bad3192777e561b362e8467c213231ef9f      1
    324 0xcc3a32b842ac1715cc0eb8fc5dfa890c9d2590cf      1
    325 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    326 0xcc9c8cd1091e83a1f433de1cd91b167a41594e6e      1
    327 0xccd021ac40448e74641c34e71aaf617afa951ba3      1
    328 0xce716837e7b9469e7da0816a8a7f3e487a119186      1
    329 0xce71b4e67b1dfedb22bce54bb0f9b7b9364948c0      1
    330 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    331 0xcfcfdcc985aa078e8b4e086c9355a3ed888197df      1
    332 0xd1598c76c78acc698a5241578ea0b21afb29ad44      1
    333 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    334 0xd1d333778b71a1015c0d529f4603f4b7296f5a96      1
    335 0xd3119ac49ed87c594a521f9930be6d0c6b2cbc71      1
    336 0xd31a84c20bc430ad75e6a1903e7ddbee52211072      1
    337 0xd3fdc98d610349f5c94d9c341d878403721c7d34      1
    338 0xd442deb96b90e3b3135653705c3ef1feefb346be      1
    339 0xd57d9f1b54e30862889b2ffcff36bda1eb8eaf8e      1
    340 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    341 0xd5927446a85b2dc8b28144f608de76c98fbc4e2a      1
    342 0xd5a220792f280a5f939d133e2fa7eeb654aa2865      1
    343 0xd6cd34f0d27d3942fb7aab193566bd76f694bf59      1
    344 0xd77d92f3c97b5ce6430560bd1ab298e82ed4e058      1
    345 0xd7a4fc0ea7b46781297d69e299ce944e4318cfc9      1
    346 0xdbaf06e16a7f1d997d61cccce5db252a25e519f3      1
    347 0xdbebeff782fcd9bfcdecc2e13fc54cdb92eca335      1
    348 0xdcbd73c88cbfe8ed9134e3ee1d31c4e9f09f946b      1
    349 0xdd88f21e5926a1f1db0ea88ded093323d63eed8b      1
    350 0xde37e77af4cf141412701d0840befe201d10ef54      1
    351 0xde58e58baac90738dd9d941fe01199ac8d36aa54      1
    352 0xdeb9fdd1711a826577347f46a2edee59dc143589      1
    353 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    354 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    355 0xe09d8f5f02bfa8a5f72eca6ae706ccbcb7351911      1
    356 0xe13a7547d2f5dccf58d0d80b0d9c3ced356cd228      1
    357 0xe2f9295a64c315624db7fec31d04d37cf25bb060      1
    358 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    359 0xe3b7f972adc1f0ffd226d155c77a842d9cc8a791      1
    360 0xe3f42af201ec36992a6b89e0ad6f814b901d1128      1
    361 0xe41fe3584832bf859e2919413cab1c810af7a9a5      1
    362 0xe44436ac79b3d92631395de6d86568af927c627a      1
    363 0xe44591cdb2a3d74227cc30b420265ef6b587e770      1
    364 0xe4555076f27dc238c654c13710fb57435a7be2b2      1
    365 0xe48c9423046d888ada50501cb4f5fc7754bb8a81      1
    366 0xe4dfa2e06281e12e1ae0ffa358e8057450b34f63      1
    367 0xe74abe3bc87a4cf34acfd7c548208700c0e669b0      1
    368 0xe77e6594706a08177f4212217d46a13561bf5dcd      1
    369 0xe9d53246209e4cd87dbc5d1fee71f44bff89a611      1
    370 0xe9e30d2cb818169c1a7e3a3343822f5b714d1fe7      1
    371 0xea1728873e79feb8071d96f3ae38097492020d35      1
    372 0xebd28bc579eb43d53d5ae799c29293f2861e8f9d      1
    373 0xef30fa2138a725523451688279b11216b0505e98      1
    374 0xef51c8be528fa9ad489fe06ab9f87bcb927bb4d2      1
    375 0xefa2d63135911e0936d2dd6de14b5b072b6081b5      1
    376 0xf10045a32920082f51b4343ed2dc4bb1944355d8      1
    377 0xf159dc86d3989f76e3ee343bb30e672bc081bb88      1
    378 0xf2b506a799159ac4add1ad7182d1635b6cf5dc24      1
    379 0xf38f032591adda680da137fae9c3ed56825742b4      1
    380 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    381 0xf5394ef1cb6354f33dde282b40c62b32ef96dcb7      1
    382 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    383 0xf6b5cf69cae094d636bb06ddfeac7a386631b449      1
    384 0xf6bd73c1bf387568e2097a813aa1e833ca8e7e8c      1
    385 0xf737674eb90de5a376cb947bfd6e8f63635bbfbb      1
    386 0xf7edba699803841f11b2354da9bbfe4f3705d283      1
    387 0xf7fdb7652171d5c2722b4cdd62c92e90f73c437e      1
    388 0xf8689018b84559feae7dfa894acf2869ba12ff96      1
    389 0xf8d4cfa38bc95eaddae6cb9d0efbb803e8541f96      1
    390 0xf8f9347ad38f607e2c510a211a563cf5c5f52e89      1
    391 0xf9003416242c9c7f92f404bde23986e2d91a9982      1
    392 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    393 0xf95f8d46c678f138406b15a795e959365d94a1eb      1
    394 0xf9ede7e6347382c18771ed98e734a85d235ed4c1      1
    395 0xfac137e6753b1c7b210cd1167f221b61d6eb4638      1
    396 0xfb032bde022daa885030f5055a355d1b27f9cb48      1
    397 0xfb2ec6543d97808d8a00fca199c39022b0b3368f      1
    398 0xfb7f7ed923ff00a7bbe14b09b8180e2dfdd5de46      1
    399 0xfba206e73709f614e5a85aff27a98692d4f3c579      1
    400 0xfcf15cf709c8e19ba3c8d2ae397cd761f92102b1      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 70 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     8 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     9 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    10 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    11 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    12 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    13 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    14 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    15 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    16 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    17 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    18 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    19 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    20 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    21 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    22 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    23 0x59068075a799594db03c0255eed68e8e121155c8      1
    24 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    25 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    26 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    27 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    28 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    29 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    30 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    31 0x69e68074f1aada957edd39c5eae0069973343f30      1
    32 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    33 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    34 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    35 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    36 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    37 0x82139687faae8a29851902783e02e699de0e0846      1
    38 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    39 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    40 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    41 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    42 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    43 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    44 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    45 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    46 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    47 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    48 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    49 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    50 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    51 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    52 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    53 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    54 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    55 0xbf814810b44978de273191fd612aa47f7b69d564      1
    56 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    57 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    58 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    59 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    60 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    61 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    62 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    63 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    64 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    65 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    66 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    67 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    68 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    69 0xfd22004806a6846ea67ad883356be810f0428793      1
    70 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Allow Memes 1 Phase 1 (Full Set Both SZNs)

``` r
c(allow_memes_1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_full_both_szns_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 49 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
     2 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     3 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     4 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     5 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     6 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     7 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     8 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     9 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
    10 0x23602ca06e977c86339ffddad74966e824ab691e      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    13 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    14 0x3876be5be4998adecbfbbad26604a762467e7f42      1
    15 0x388160a99390392278afdba240046b8b5e73f77b      1
    16 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    17 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    18 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    19 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    20 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    21 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    22 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    23 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    24 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
    25 0x82139687faae8a29851902783e02e699de0e0846      1
    26 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    27 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    28 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    29 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    30 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    31 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    32 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    33 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    34 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    35 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    36 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    37 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    38 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    39 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    40 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    41 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    42 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    43 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    44 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    45 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    46 0xe25b24cebed3055236e369570a437a99e1d32602      1
    47 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    48 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    49 0xf1f476f144df01480297dca47efca565e8b0c9f1      1

## Allow Memes 2 Phase 1 (Full Set SZN2)

``` r
c(allow_memes_2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_full_szn2_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 187 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0216ac50fdb6b46f6b74254b84ecdd1d431d670c      1
      2 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
      3 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
      4 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
      5 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
      6 0x0cb5b9eeed8d51ae57e70e46b2a3596b94ece6c7      1
      7 0x0dd38657d7c8024e7d62bde3d0423ca34769be50      1
      8 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
      9 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     10 0x111b863c2f7d1833d8f53830647c260169e99626      1
     11 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     12 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     13 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     14 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     15 0x16b5ec9e34bd0a916822d6785b1c600ac911d6dd      1
     16 0x177a8340bddc2041cc362aabf543dee4e394e3e3      1
     17 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
     18 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     19 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     20 0x1be1eac561b34c9b5e45eba9c3fb51fc5fc2796e      1
     21 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     22 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
     23 0x215cbb1b60e2cc1f1e8626cadca386b278aa0dee      1
     24 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     25 0x23ae72f8336aca747ef02d596403de56cca489fb      1
     26 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     27 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     28 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
     29 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     30 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
     31 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
     32 0x2cac9e6d1d630a35cb8019779daf4c5fd96135ca      1
     33 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     34 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
     35 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
     36 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     37 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
     38 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
     39 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
     40 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
     41 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
     42 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
     43 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
     44 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     45 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
     46 0x429683282e5a28b81aab1f9126b8d5e4847dfbeb      1
     47 0x42e5abe19ba467b0b32a4d042d3dfd4ba2e196af      1
     48 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
     49 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     50 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     51 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
     52 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     53 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
     54 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
     55 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
     56 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
     57 0x527bb834cc5c8ff730c673880e51372282b06e14      1
     58 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
     59 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
     60 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
     61 0x58059014c5952c04370bcb88a2e0503e9eafb209      1
     62 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
     63 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
     64 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
     65 0x64f02c24bb510e27b5eaec705eaa840ff7b4df15      1
     66 0x66b280b5778c35c719209614428caddf00aaa3ce      1
     67 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
     68 0x69cd43dd4ecf3a076b1b9d2cfd41589987129bc0      1
     69 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
     70 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
     71 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
     72 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     73 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
     74 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
     75 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
     76 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
     77 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
     78 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
     79 0x7729cfd6841ec449c20fa7d3970d2e0d035c856c      1
     80 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
     81 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
     82 0x7b8a6e7777793614f27799199e32e80f03d18dbe      1
     83 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
     84 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
     85 0x808421753a181e96812796b7ab43d3f356cc5a77      1
     86 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
     87 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
     88 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
     89 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
     90 0x86c71b7d27d00d7814a763f9e6f16fdb09f7e4df      1
     91 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
     92 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
     93 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
     94 0x8d12c02671848b17c18322027a2578ea7afbb702      1
     95 0x8d5e59e11838cff72af5fb0681d96a9136ad0604      1
     96 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
     97 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
     98 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
     99 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
    100 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    101 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    102 0x9e1e3857fb2484379858b9daf230379015a7a100      1
    103 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
    104 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
    105 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    106 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
    107 0x9feba45425c51b03890604d6ad51f4ee7c9d4866      1
    108 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    109 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    110 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    111 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
    112 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    113 0xa97204eb6b0f3e58e8acff48a2317511e7628949      1
    114 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    115 0xac9169946d4f6fea622900fa6aa1812d5500bade      1
    116 0xad43d2aea785e775bd38b5bbf4c5808572758373      1
    117 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    118 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    119 0xb1c11a879c47debc22e3816e7b727fc8bbe3c8ac      1
    120 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    121 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    122 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    123 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    124 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    125 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    126 0xb8b8ce5e3d0ad5c0169e130c64048b6d3cfcd343      1
    127 0xb98af149b33dee33c97650e877a497bd6a934d54      1
    128 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    129 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    130 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    131 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    132 0xc03e57b6ace9dd62c84a095e11e494e3c8fd4d42      1
    133 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    134 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
    135 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    136 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    137 0xc522289168311a765cf17c067f0118578c99cf08      1
    138 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    139 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    140 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    141 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    142 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    143 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    144 0xd02cf646520827d74d4ff0435f96220819772b4b      1
    145 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
    146 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    147 0xd1dd467f1630be10d67205d6c816429f8ee49124      1
    148 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    149 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
    150 0xd4e791d799b8873d86ac06a5fedf2f1a75f639be      1
    151 0xd5ec003289265705727b622f1700fe814e54ca67      1
    152 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
    153 0xd72c8dd33954e8aa53d5c108906b751ce4b2382b      1
    154 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    155 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    156 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    157 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    158 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    159 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
    160 0xe60458f765bc61e78940c5a275e9523d1f049690      1
    161 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    162 0xe7f31a4516dbebc0f443a8d4ac8b22d1a8f096e0      1
    163 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    164 0xeac5f9b3cd48123a69fe69ce93a7f58100a56552      1
    165 0xecea65d22233e7beed752a06c46cefc6c90a1d7f      1
    166 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    167 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    168 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    169 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    170 0xf12e159643edeeba920518cc614820ab5726335e      1
    171 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    172 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    173 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    174 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
    175 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    176 0xf6109779752d1fdb8b6063a158bed1b02000b700      1
    177 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    178 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    179 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    180 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    181 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1
    182 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    183 0xfd17019d6a7ddc7ad585afa68dbef71084162601      1
    184 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    185 0xfe7ace0f186a54c0be46f992dd3072e0053a1010      1
    186 0xfeea8258077cc06444679958185f4198dd4cd324      1
    187 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1

## Allow Memes 3 Phase 1 (Top 500)

``` r
c(allow_memes_3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_random100_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
      2 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
      3 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
      4 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
      5 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
      6 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
      7 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
      8 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
      9 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     10 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     11 0x1af369bc1069dd286ff59cd69553550c07e0dd05      1
     12 0x1c172d05b75178fc669d74407243cc932030f139      1
     13 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
     14 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     15 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
     16 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
     17 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     18 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     19 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
     20 0x313c8673ff69dfd7be2a46bc540305d3e2d233b0      1
     21 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
     22 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     23 0x3bf55d8aad7ca0c5cfe1f697d7783437b9e034fb      1
     24 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
     25 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     26 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
     27 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
     28 0x47a79ee280b4a6a8d68a3ba16e4082a765a2b20f      1
     29 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
     30 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     31 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     32 0x53b2da71db2e266882a465d9aeeaae3e8beeb9ee      1
     33 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
     34 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     35 0x557bd9e829f35f1d853c2c85341458892753fa1f      1
     36 0x557c60995797fa7b47be105227a2e46148d85750      1
     37 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
     38 0x5d25087405105bab12624c73488ec186066a6376      1
     39 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
     40 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
     41 0x614b89f072ea263a9387460963142e73548fbaf1      1
     42 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
     43 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
     44 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     45 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
     46 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
     47 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
     48 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
     49 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
     50 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
     51 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
     52 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
     53 0x843708d85621273f3bbc643b348da3a60d5b0334      1
     54 0x8854b06ba346a703a3c043e2e7b8822db3ca6b3a      1
     55 0x8856dcf8d1f9be083a25251b0149ad74f971b806      1
     56 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
     57 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
     58 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
     59 0x8b4567fa8c4715c27a682215a031033a8b544206      1
     60 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
     61 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
     62 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
     63 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
     64 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
     65 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
     66 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     67 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
     68 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
     69 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
     70 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
     71 0xa749f12e124790d45e62bcd3f7bf1d2218f2f21e      1
     72 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
     73 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
     74 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
     75 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
     76 0xb196931ec22517b0510705eb56d5652fe73877f0      1
     77 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
     78 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
     79 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
     80 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
     81 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
     82 0xba12fda058a14eb03c14613601c3a30d6f955196      1
     83 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
     84 0xc45920062985116eaac6589058ed337066d6f2e6      1
     85 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
     86 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
     87 0xd0b2d2d86a45bf2f13a3af83df6e7b5fe98050f9      1
     88 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
     89 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
     90 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
     91 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
     92 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
     93 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
     94 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
     95 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
     96 0xeeb9148a32b45672e16591ee18ceabc773b099b5      1
     97 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
     98 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
     99 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1
    100 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

## Allow Memes 4 Phase 1 (Random 50 from remaining)

``` r
c(allow_memes_4_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_random50_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00668bd79ede077b99bbe1c4db59418bc333d4cf      1
     2 0x17f86464648722a5967eb8ce43864b7903bb4ab3      1
     3 0x1cde9d1d8dfbf4dd793a14823d554c06db80240e      1
     4 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
     5 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     6 0x23cd0c136f4bbfac9c23eebd97d5fcfaf1230582      1
     7 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
     8 0x26b0f2d8c16e0076d9c00ff68bda5e75563bd215      1
     9 0x2e769f9cb238e5d93fbe353831c80bc92ba239d6      1
    10 0x31069e8cf34a114317989ec3f590fa5fe48efa3d      1
    11 0x36a0d4dca6d768333f916368b73bc689438e18a1      1
    12 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
    13 0x3f0fdcd6f59fc4ff885006678e4ada78de1b0dd9      1
    14 0x423a9a8aff6d51d7aae8055ffa0072afd26ad1f0      1
    15 0x44922eafa08f8ace07fd68b60713e399360451b3      1
    16 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
    17 0x456f6e34ac23ebf3478f3ade23c7f633c81bd461      1
    18 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    19 0x4c4b8d30813a59ce6a3e590f745ab9815a206858      1
    20 0x546c553b189d5032992e3095df53efeeb027c9de      1
    21 0x5c055c05126b8bd0a16596e79f76308098ef46f3      1
    22 0x7083ca77ad912ebdb3dfb3c6acd76556f8d6a5d1      1
    23 0x70865b92b31676ac6907159be3dce1ed22d62707      1
    24 0x740a92667afbe58bd1cf086625951f9a7197d2ee      1
    25 0x79fb29852d012e00ebab0e253400ddb189475ea2      1
    26 0x7a20b6dd971673cc75f507c1408c13140c11b472      1
    27 0x7d4d3e72d540474e24bfdd19644e8df14581664f      1
    28 0x853c69418605529a68907aaf7789270e3cf69d97      1
    29 0x86d09318844786f26df5663de29306e673e668d5      1
    30 0x8f9c8d7f8d44fd2486bcf0605efa5fd0c397a658      1
    31 0x9760fed718c26df146e0e47e259609281e0d4954      1
    32 0x9d8945e2c08751ce18bab505a57073c8de16cd11      1
    33 0x9f910e638591f609bacd3a11f709ee83a3828a8a      1
    34 0xa222204acf1be4077d34102fab38a759060b77c2      1
    35 0xacf5a4c1a6e990f72409da5c26a0e8a352cdbdf0      1
    36 0xaf9ec6296b3fa072e72f41d2040a0d68ce2a2805      1
    37 0xb9cd01bf2680bedd1bc41b66d27840d06bf4beff      1
    38 0xcc3e5f175b24664291afa6e9d551117f05fe8f20      1
    39 0xd26ce943636ca63a1810886fd1444d3dcaf738b3      1
    40 0xdc19d539369c3da9172de453c05bc503c9ac6bc2      1
    41 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    42 0xe41fef6d3b5f174ae306b17e80e023915d9ac093      1
    43 0xe66d15146b16cee22f0231dfdc830e7e56c2d4c6      1
    44 0xe78f56dbcb2f1187ec5091832a967fa0a872074e      1
    45 0xed4a660898b0ef71bf1a027287c7320ce4357785      1
    46 0xf06fc22f0f9d15a271e18aa216fe183e20a7a092      1
    47 0xf3bca892960d23510917897b634ffd3124702bbb      1
    48 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    49 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    50 0xfb89213242d1043e560b4d0232a75b4f524e74d1      1

## Allow Artist Phase 2

``` r
c(allow_tokyoluv_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 535 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00c67d9d6d3d13b42a87424e145826c467cccd84      1
      2 0x018e2c4109d927ea04d618c34dbe5f5c82802f5e      1
      3 0x0199e49aaa8226841bfdd9ddb0ba61ad0b251399      1
      4 0x01b8744f70ce3386aac11365516b231a78004198      1
      5 0x02062b8727cdde62cd784382c60d738fcdbb736d      1
      6 0x028cbf38bd7354f465cf9a65e15b6c42f6ae3721      1
      7 0x03a14b9a45d5c29a48e4a429b69064d4005f002c      1
      8 0x03c2a1f9d098aeeea6676fefb959362ec5d65ab3      1
      9 0x03e0e647051d14ec16b44438893913f048810cdd      1
     10 0x03e9bfa01bda69b382a34c9d7eb70fd4c56dde46      1
     11 0x0409d28e409448e543c610b369ee7d2b58c15f57      1
     12 0x04ce45c82e68fd4b0b11d5aa592a171068e1a6c7      1
     13 0x0524c91701fcc461c90b3120769b5f295f706a16      1
     14 0x05886e559734841906abc42f033a6cc3e7a83295      1
     15 0x059e0ada7bee6195cd2f008471c4a0431057a434      1
     16 0x075652098fb002791f59d66c59c09ffddef66b1f      1
     17 0x07587c046d4d4bd97c2d64edbfab1c1fe28a10e5      1
     18 0x08a4f7c5b0021ca15d65e410b22825aadc84aba1      1
     19 0x08fd7690f92ba43a923e7fd70977a74d01b5773e      1
     20 0x090c984cc693b991d7cbaeddb43cb75f13fe5f2c      1
     21 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     22 0x0941ae5a513cc77c99df2b8f9f35281003d49510      1
     23 0x097cf6c60650ea25a3443b9fac9ab4b60cdda24c      1
     24 0x0a35d3b6cf5361f85a3beb81828fe40c1b4cbc9a      1
     25 0x0a828953f4a30f097e6217bcfcb8fc4f7c9aa110      1
     26 0x0ace76b681084e0f7fd9fe4eab763275d43bec8f      1
     27 0x0b20b9bd6ea6f9d75c3eb676e703056e1b7538e3      1
     28 0x0b38bc8d3185718ce5848e0bbd258d419be16bad      1
     29 0x0be2b32988328ff33f687ac869530291c883bcdf      1
     30 0x0bf3f8648b1820ea304267f65ba73649632f3292      1
     31 0x0ca43432e83defb4cb2856c0eccadc8507db8142      1
     32 0x0cda14d6f49fc599107904f5f677440412b905c2      1
     33 0x0cf29c333a84064723b3c8d4630a5af539f18e3c      1
     34 0x0cf4b46094df84962e36929171ec5f87885d4b59      1
     35 0x0d8e3e9f01b392e769c1eea44c4736c2d7f3cf96      1
     36 0x0dd266d1246e571e58e6e4477f33962fa633b9dc      1
     37 0x0dfe9ec2f57c3b95653f47da96b49861e4010b36      1
     38 0x0f07a987346c520cf7bafcd2336976e5c77866d2      1
     39 0x0f5c6d25f47c3c26c2fc128f8e245076a8b6fd85      1
     40 0x10c5d48b6b4b64d2f6c5e58dff55aafabde17709      1
     41 0x1141a2881050320c927aa04d293530cd2db6870b      1
     42 0x118fe9d1dc8efa98f3bf41618d9a3d7f049a61b1      1
     43 0x11c632014d1cc64f73bc84e4e9768bf0fb5ac7f6      1
     44 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     45 0x122a1e76f9db0cebb5ab147e48416f87c72b7fcf      1
     46 0x131415a008df1869a279f781f2ddc6cee795de27      1
     47 0x141e149426b0c40dfa55de98452f04b7fb1b79f9      1
     48 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     49 0x16278ae3bb0eeb3d04093a16c1290ea8e2d9e879      1
     50 0x16304dae95525d603e2c15ab385c5463a6b2113b      1
     51 0x167508e21d39c605bd4215271fda97889b6b9355      1
     52 0x16e9cfe34541d8601da1fab1707fe2b1b867887f      1
     53 0x175fc6de1db2ff5368726f7bbc9934b9f2333917      1
     54 0x1796e5666d3eda5c95fcdc13d9e0bb86dbd47db0      1
     55 0x180b3fc00745c68ef5c6d53b3f9cfa5526a7494f      1
     56 0x18a72ce31b61b98cda91dabbabaa04d61d9a0e6d      1
     57 0x18d8b581da0df8929746a9e74d74699271bab8f5      1
     58 0x18eb4700b3949143a906e6b0a7388e45cc140b93      1
     59 0x18f8fdecf6b053619ab730ff34f312b31efe6544      1
     60 0x197008a1d3e26a97a19f46c121482969cef95b7d      1
     61 0x198ad6c547d20d70f2f656a4f48e6c7cfb7b4325      1
     62 0x198e363e2e7d58f521960e4175a7dfe0f59936f2      1
     63 0x19c6e5019a785c90127d093d6364084c958225ec      1
     64 0x1a2cfb4e1ed487c94621c4c8cd73889679d09631      1
     65 0x1a730a4a024609b180aaa1d6104a6c9c5daead28      1
     66 0x1a96af39c6f2a05686843d2c1f3ca5c8364cc48b      1
     67 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     68 0x1c7471f01889137b32258b8e96b365f3e79a7b07      1
     69 0x1eb322c016815ee5b29c071586c1b75be5934576      1
     70 0x1efef24c392447c130066d466aa7e292135501d5      1
     71 0x1f9723f73daf5a0c6b75ff0496689ca86dca58a5      1
     72 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
     73 0x20962c1d14718ed64b0b5b13c87b04b8d58451b7      1
     74 0x214f85fcaeea5930e090571fe1e7873cf851cba9      1
     75 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     76 0x23a650462c3502a03e58192c9253437705140f8b      1
     77 0x252ad4c147630634170971fe0bee72feaf7dfcb3      1
     78 0x2534134861d3b0e9779670f6482901244887d676      1
     79 0x25665e8b95792c23d6167d2d0903fdc6bfef87e9      1
     80 0x25d50ce5c3c2962b1eb22963070432c8ace816b0      1
     81 0x25e12fddebc79898e7d022d552bc5254fbb05aeb      1
     82 0x25ef59fc52c7be6aff54d44cce1a58c11ab5f57b      1
     83 0x27ae9d926322d8a8ffa6d3c7e3d462af6b4a2a60      1
     84 0x27bd2b199eaec39bce5604e030259c0bc1bdaaa0      1
     85 0x27e6d7222b89af5329bc4675558153c1aff8efae      1
     86 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     87 0x28f8056b6d87f3a92373a9403fbfea1a2da51904      1
     88 0x292f3ec949fffe6089f912302bb613f572d87c5f      1
     89 0x296c2d4daaef7b105ea4482a842e1cc882583547      1
     90 0x2a3a77b570bd059e85461f67636fcf69a24864ea      1
     91 0x2b28665699b37021a0d5fdc942fe1cd2da902234      1
     92 0x2b29c4e2609a09115a2562826a491546eb2e689b      1
     93 0x2c0e519cc4d8af8046f26c0498a6942ed0aab53e      1
     94 0x2c31159f1497aed2cf9c028e55416f3050db1dff      1
     95 0x2ce9b35ae3ddb0ceef52fb41022b1d38d701b25f      1
     96 0x2d3ffd5f6624e1bc063c1fb1722011cda927f5de      1
     97 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     98 0x2e027b06d40a95a7fe69597e740192e3d5774ef4      1
     99 0x2fcc992f92b1f40c1283a5731a4da6955503c235      1
    100 0x2ff0ad27418a9d211cad22e0c11a1255abb5b3aa      1
    101 0x300be37f3e66a7b0056017679b8c0efad72ac8ef      1
    102 0x30183f3f871fb12fc3025124f02e57f3b493a671      1
    103 0x3039fb04dd8c5d0d66493df168cd7aba92c57154      1
    104 0x319af5203839970ccb04d2eda95c427941c498ae      1
    105 0x33d704d1347bbf81c05104bc41bee19e83c02205      1
    106 0x352b82f0b4b6ad64e4d99fc50d63a7f43e5c8a23      1
    107 0x36241340ceaee8c126d398719b6eecd96035b45b      1
    108 0x364f1d3212f98796761e5e166a1aeeb2b135dd3d      1
    109 0x374b0c9e9c3fd1ed77cad4b598d2efbc9a985843      1
    110 0x3825fc868e8e66ed3764bb588859dddab6f04a74      1
    111 0x3841ce1df8e99708ecca3867d2fde84696c424a6      1
    112 0x384bd7b5caa87e2c75db2d6c7a22c8ea914926fe      1
    113 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
    114 0x3924409e89c3d3c43712d44975744a69e805661a      1
    115 0x397dcfea2d1e55ba8b61dff2d20aea6b8b893997      1
    116 0x39965e3a41d624a82452323492470b81fc85673f      1
    117 0x3a71c160022e290065a4252ec826f25a86885a55      1
    118 0x3b4888e46ed13e3121409bf39a4c9aeada7f558e      1
    119 0x3b8ce093bae0ebeb766a4a0c638521189e1fe1d9      1
    120 0x3ba74c579e12b9e25b8c55dbf63c22b0e8d86d1e      1
    121 0x3bf130738c8babd9bb9a2a16f95fb54de4c720f4      1
    122 0x3c52433d98ca1e022112a94450c46e851fbec47c      1
    123 0x3d6feb003df39e2dbee3448fa88b7ce32be413f8      1
    124 0x3d79e7dc7fb949f60c1660bab6a100aa6ee1311a      1
    125 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
    126 0x3df6c1d54ad103233b3c74a12042f67239d69f70      1
    127 0x3e5cbba78dd36f651cefe21951de8f153ab1f8d2      1
    128 0x3eef4866f1fc4e668531eec2dfa41f2d2923285a      1
    129 0x40462752bea372a09ae67637c321e87d53807545      1
    130 0x40983b311f5a1666932c2c8319606caee7c62afb      1
    131 0x40c8cef5dac8e6d0e4e11d1856c155fc4ee31046      1
    132 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
    133 0x41ad67f7d547c3c03b45a1fe1239d6ce390638ca      1
    134 0x41c78684c1c25fa720866d856438c06cf1bc6e83      1
    135 0x41d368a53af7afba2eb47ac9bf1611632369ecca      1
    136 0x41eb782baeaa5fdf4b36e16809a23a895167da40      1
    137 0x4277a2ffebf046eea19c4bddd5177435e735f7d4      1
    138 0x42d3b1e30f39191f6da2701e31cfc82574ea14d5      1
    139 0x42e250824bdf98f3e450784b0c1cdc3ac2f157e2      1
    140 0x42e61f741cd62a9a3fa9b568e2c990591941e0c9      1
    141 0x42fcbd3e6b8cfdac2ff230ca323062a6ef415237      1
    142 0x433746618b5bc2f7dc85d022915d2908baf5d24b      1
    143 0x4398193d60dd9237097880a798dc16fd81802646      1
    144 0x43ed63322703ca6249ddd5a094d9bee0a3e8aaf7      1
    145 0x441ee5e616eefb10116d25fd2c69499eca8e9126      1
    146 0x444441e930f1335cb1c50702e0dbdd3b6e1e70b3      1
    147 0x44c2a80f8b924858db469a5b35128908e55c531f      1
    148 0x45492c6bc6ab97e4320e6f844c560be62737a303      1
    149 0x45ab04c54264f485eef8db0c20e531e9d37cd53a      1
    150 0x46aa797e862fa52833b6176269bf028460e79659      1
    151 0x46ae568842e3f3d2bb7e2a4f6ca8e38692c6ab86      1
    152 0x46f1fc698ca29143405e311833aa937555340d44      1
    153 0x475e22217d88f0f852a4ed3d0499e728fef7ef25      1
    154 0x47b553ade978348ef88fab434249b94a86e8f76b      1
    155 0x48910a64005cff019885bd7b277e0a69a94ae11f      1
    156 0x48b6f46635fcb3559fd37289256ac72c8425714e      1
    157 0x48df5e684815dd7fb42af22508cc59f9e4237c96      1
    158 0x4a29367c5ae9f84ef03e447d1f7dee8e6b16229d      1
    159 0x4c775fcde221a18c82499ca73058e3055373ffb6      1
    160 0x4c92b05b5c38999b3dfccfed9aaebeb727a36f8b      1
    161 0x4d477f1aabcfc2fc3fc9b802e861c013e0123ad9      1
    162 0x4d68cc403dc465d3ba7b20fa67e8972dfcafd559      1
    163 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
    164 0x4dd102b9b089cac422707d85a630564a294ec50a      1
    165 0x4dd5a4d1fcb6f8938aa8ad070aefe1b19e8f9c0c      1
    166 0x500439824be448cd35587b797546a7effea1e65c      1
    167 0x50ba57070c7c8bb70795014e68ee0269d328bf3b      1
    168 0x50d7f6eb6beebe888556a124324a1420652c96ac      1
    169 0x51097fdf7b9ba09e578c4421b99b427097b055a1      1
    170 0x51635999dfa5b6ae3a6cdaaacceb23d2530cad6f      1
    171 0x519f95380bed75a307dd16d53842d83f342efa3d      1
    172 0x51cb17df6efc574428bd70f03b9f65401478f691      1
    173 0x525c25f7a86f163ee6088be4f724c8ab74af3674      1
    174 0x52f8d5518a05366989b3baad9c8e1129ca317fc7      1
    175 0x54a2118c10bc3c2d62651d2355dd67bd62807aa5      1
    176 0x54d4f9b909e03eed30ffe7886e064867562fc130      1
    177 0x553f458951c0cf43875a378568c8c7ab93ae091e      1
    178 0x55ac2637018eea60526edd62503f9b7d103882e0      1
    179 0x55d647550ac4dee50e00edf13debee5404f4c452      1
    180 0x56f158bc2ce451dd4264db6c2121afffd0723669      1
    181 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    182 0x5843236ce0d3e08ecc8898a995212ab4c4a11107      1
    183 0x5871db784bcd396f6b1a7fec2286ca7e74864360      1
    184 0x589a8d041c6ff9af5aa27fcfe401cd6d1f188376      1
    185 0x5979de03f5eab4ae548a27ae9e1a014f23f10bf8      1
    186 0x59843418a92117ea8ba8f07a9a2c38dcb651331f      1
    187 0x59877d2ae955ef54a6c99a22357bfc00815181f0      1
    188 0x59bd24f26d7b499fc6f3eeb7dcc204327048d4f1      1
    189 0x59f3907ea8599428632dbf89c825ab89217a3528      1
    190 0x5ab2d1f5069dd2f9aeec3b0a8e923b1cdbe7fc44      1
    191 0x5c5b6cade3f45fca78aac5a9877f1b73c51300e3      1
    192 0x5c7c0fdc44b44673f070535897d1208104a28950      1
    193 0x5c8f4180e6e61a36e06438dcbb314b5a758453bb      1
    194 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
    195 0x5efd15328ae945ac756e183fb6fe9c1d96e9d434      1
    196 0x6010abe29a59fa938e8f71814fc91a6cd37c18ec      1
    197 0x606eaf6afae3cad042fbe7d8d32aac88935f3430      1
    198 0x6104731220a7e4918d9e4a06a46df1f12c1f40aa      1
    199 0x617523a19894c929378e4007ead12689c4852038      1
    200 0x61fd0d043d519f5a2bd05785000f30db96809429      1
    201 0x63243f39dc85f5b8d499bcf104b337e9a44bb642      1
    202 0x642c866adad5841049477ee85716f2396b468e61      1
    203 0x6439543a2ff1d78d25abdc8daa75bb004e210183      1
    204 0x6478c54d7e93801950ef4970424d2e84bd1a7ea1      1
    205 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
    206 0x64ad18fd2cde41578d231955c98a714f8cbac239      1
    207 0x64c0be5f58b4da7253463043359432e032e3edcc      1
    208 0x64c2838ae4ea458729fbf317c7bf5c47cd52b887      1
    209 0x64ed28868990b8440bf2de0c62747a7a13393550      1
    210 0x655973191158a71544666211443488b2798e2576      1
    211 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
    212 0x668248df4595e09aa253b31478312748078f7a20      1
    213 0x66c3d1948edb3ef275dd7b7101788196e22a372d      1
    214 0x6739bf01069b4b387ebc3f06e282b2875cef41eb      1
    215 0x678d52b7b77fafa92e0a6886d18a009e3fbc6783      1
    216 0x685403a926846d4fd11ab93b75d5fff5a7187190      1
    217 0x688976844782ca545968d368060744900c5f49b7      1
    218 0x68f1c1b915ec238cec8851cf5c38b98b74560121      1
    219 0x694d9215a92fcb234e582666ce17ec6a7fbdee2b      1
    220 0x6991011230879e8fde3249fb49da8cf14dc49c32      1
    221 0x69e1c391e6fa8df94e585b5c5970a1ea51e15db8      1
    222 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    223 0x6a90eb71a68786636b941006ab3741abd0bba462      1
    224 0x6ad5f126397fe17760a5636d1febfb3eca746d9d      1
    225 0x6b8ad90480d73dd77cd719ca637c0f0289b6665e      1
    226 0x6ba228524d45c772d7643bb60c6bed2cd3dcf053      1
    227 0x6cad38cec0128055bd5768f729c9d5c85ca29470      1
    228 0x6ceef3321f698477994d1cc18a5f9fdd4f210dba      1
    229 0x6d6832a9fc03736ad15445cb60896d51e15fb2fa      1
    230 0x6de2fba4ba6f2f6672eb9c53f5f58e6e9bed3ab4      1
    231 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    232 0x6f47ad567a09603a2812727a95528eba0fc60375      1
    233 0x6f54c650190b44ac9efe531380417ccf12d2d06a      1
    234 0x6f745aed75d140f5c5a7e610601cfc4615a818be      1
    235 0x6f8f51f318949f337095d397133a424e28908eb3      1
    236 0x707990ffe1408c414a0d3196453b5d15d12e2357      1
    237 0x708e33ac2694052b51d4dd3db1a29c75d0e2fe85      1
    238 0x709415e0d03beb6baf26487ea9d552da35133ddb      1
    239 0x70b1bcb7244fedcaea2c36dc939da3a6f86af793      1
    240 0x70b89a46170eaba5b7a65ea6c8015db58c115dcd      1
    241 0x70db2dc7bf9a1fcb2fbccd9b68b1a1e0b8b5c9c2      1
    242 0x72365e20475ca0ac6423f71b5da97dbeb8bb6c20      1
    243 0x739d1cb163ee212c45dac9ed7a063395465200e5      1
    244 0x741924bb54e7cb7e4d031c77fc2bcd1a42169c4c      1
    245 0x742eb2a4ca3ed347e55c0c254a0e357378cc9e79      1
    246 0x7491d690416f7c674a7e45b3572794478a91d3cb      1
    247 0x751ad3ef07c5f3b53cfae667bcd16077af41ad15      1
    248 0x75968622f9f7fa7550699d60394e0dd4ecffcda5      1
    249 0x75ac72ab43ca65210507e12fb72f6ec3cccb09ff      1
    250 0x75af4e0144fd4c0b4f7e4ccebfbc2b9e673c1d85      1
    251 0x771fdea15b160e90c4353505a496d227c5e5482b      1
    252 0x77432b9c470427c60469aa32eacda663c6bc3288      1
    253 0x77e1f27453f168bee1e58989bce0569cf173317b      1
    254 0x7869363b7554697703f0cf3ab943af638a8b250e      1
    255 0x7946dac167f9fa0e6d87f9821881774af52d4421      1
    256 0x79bf225fbfd40f78b1878a6d1eec1bb03df92aeb      1
    257 0x7b29269bc99fc79d08f93beb19611bd5f647c79d      1
    258 0x7cf6dc2db031b5c0a869738a682b51890756b24d      1
    259 0x7d55580ed3479db36b1d680517ffe408ef177e05      1
    260 0x7e108ba31a178cfd96a0405f68d247f96467b47a      1
    261 0x7e490cb4f141911fd0f41d3e31f3ccad19f44cd1      1
    262 0x7e71b5a8752cd9e5be9ccf7610d34b6c328571cc      1
    263 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    264 0x7f5088fd22b12c8ed8f0fa80e28de94cdaf71a16      1
    265 0x7f6d34a4fdb21729ac15aa0ae01b5b47ff2f5f31      1
    266 0x803a58b8ba6dda7ec58bc8cd53478fe6a6a628e2      1
    267 0x804e3d31f9b90a023c184eefeda1d003b082fe7b      1
    268 0x808f8fb00dc9628dfa217f6d7e51121e509d33e1      1
    269 0x810a85117ba2184d08d87d0e7bee8aedadfa9c2f      1
    270 0x817ed85ecea07d7e90f228dc3a78085b1022ed0a      1
    271 0x822e811756be053c1a38134f6086d0bbc22bbe99      1
    272 0x82829d07b06a65a15ebd273cb0b734fe39ba99bc      1
    273 0x849c47d7af3249a9726c01ecf08e95e9b0caed0e      1
    274 0x849f8c0abfee0c70ebd20ad9759d81b1e279e4ca      1
    275 0x84dad78c0370834d38e1808d022d3f99f4a0e76e      1
    276 0x84f4960bbb36f98d11b74d97dc8efc2156ee5943      1
    277 0x8572e09642c8c4941a774ee5a3534b9e3379851c      1
    278 0x864d89b2d2486d40b702ea7e8bf15e79c38e622e      1
    279 0x869e92be652a6541a8b1d6e5a34799b89a96e64e      1
    280 0x86a9c39d3f2995000bca63dfdd14302ac959d124      1
    281 0x86bef9960d9fb3330596f3dd8b28e3f311db4d4d      1
    282 0x8751768e2ff68066f5c1ce21501046c4a0359e0a      1
    283 0x8889ebb11295f456541901f50bcb5f382047caac      1
    284 0x88d3574660711e03196af8a96f268697590000fa      1
    285 0x8a1a185e75c9b57b209524c3fc0d449b9247e353      1
    286 0x8a1b6a1134628e1f3827fcdbbbdacf6a2c54c5ea      1
    287 0x8a9b6ba6df78718121f6adc81b030dcd6e1b798a      1
    288 0x8b7e3036d0f6d606ebbc596df0f8b253789ea84a      1
    289 0x8b9067dc5da386a8653a9ee953c2b57afc65f5a7      1
    290 0x8bb69d933b031d541613ca5886123ca9572c5e82      1
    291 0x8bc303246b92b797cbd83e8527677402db2b2609      1
    292 0x8c99623cee8fa0bf6e7632c8f170c6467aa54daf      1
    293 0x8ca4ae4152219ef8f4257759064dc857ca823dbd      1
    294 0x8ed3e58fcf1f2169f7b39fc1aa8d753719ec53a8      1
    295 0x8f77693857be1f1eea4d4b3bde36e2e56b533b27      1
    296 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
    297 0x902ec340bc5d5c9ac774a1989b007fc847daaf76      1
    298 0x904935e7b85844202769a15274fb45c0915c6693      1
    299 0x90d09c5e8d74d96994377486c3627f395eeafb75      1
    300 0x9112a517d9fcb115b3f387b845351aa309747c18      1
    301 0x91e333c3871c1d81ea8a879575e4cf9b20119b49      1
    302 0x9223f4702d712f6a0a8b2c8671e1f66be6cbad21      1
    303 0x923b2d973b407c317869124362515db392148342      1
    304 0x926d2562cd25e2a8988449f61271c19ff65d2c04      1
    305 0x92e0049c4cdd67512bda9ba4dce55b554acf44ff      1
    306 0x92e9b91aa2171694d740e7066f787739ca1af9de      1
    307 0x9348b8914f29b0b3ddb5b8a4bccb13af855d1e6b      1
    308 0x93aa2dd8d8bf6f648218bc3843c62f1956a621c5      1
    309 0x94042981fbe827237acf0d49bbb51fa8c1c56aa1      1
    310 0x94fd5163ffd26d716dc5b44d4c4fe4ea2598e8a1      1
    311 0x9517d9143ce88d6d592a604b477efbf1818af2be      1
    312 0x95298343ae03528a6b3c5d211005937f4987b51d      1
    313 0x953448062cbc361c4a49144bd1d43a294e4b61eb      1
    314 0x958f3ff49e0bb6591894d0e25dad67bb84c13cba      1
    315 0x95df19aef806ac62bc22ee5801e6ca7db560bcab      1
    316 0x969c65094269c90da10a9cb08390846fcbb40047      1
    317 0x9704b1537a933bcc73cb603f93849a051447fc80      1
    318 0x975421d06eb75eed183c14712ead426bb11f9853      1
    319 0x97735db6fbbc87eb32ac63b28510ab5da2829a52      1
    320 0x979030e6883454f3dff091708717f64e89a81e78      1
    321 0x97974bee58e60a50320376d31536eb76a13137ab      1
    322 0x97d1ac9e64564d8aec7d9dfe9f1371df4d1c7eac      1
    323 0x9810296272158fbe6c314407ba0e7844249f38fb      1
    324 0x98367d7b9bc02a5207859ac11f2a9e504ca729e4      1
    325 0x98ca0f68c168287d1a93f223485f4d68a21a3e95      1
    326 0x992392dd5ed820628c2e3f4bccbacca52482e78f      1
    327 0x996666de859b274cdc925eb890ebb39aadbf3317      1
    328 0x998e6b7a66aa7b3d9c0a2890d445c7792f33a542      1
    329 0x9a473e6862bb04ffb37e702b2da9fde1c6e8860e      1
    330 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    331 0x9aa5423e8793f0363941747d867f6d6b3dc04d6e      1
    332 0x9af0faeb95cba35237fcb51046449c97596afae6      1
    333 0x9b2c1174c7648569ce1f951826f0086593e23201      1
    334 0x9b717cf7313315f27e8fa93c822f3e9cda07e8b2      1
    335 0x9b7b46e87708ab66ab917129fc109b5f9ea313f0      1
    336 0x9b8dce62005f1870fe9fbd80560f989e4f08ecc2      1
    337 0x9c7b84be5d69bb41a718a4af921e44730a277f90      1
    338 0x9d4726035d9f0e4a944d39221916f5464badb1b2      1
    339 0x9d4c1c518442ad9eb23cb79a4792d508b49c81af      1
    340 0x9dbdad4abcc48610b22d56a6bdd1aa7f97171b06      1
    341 0x9e8a79d6f2cbbc84eed823fef0424c70ba04b066      1
    342 0x9ee19c090db326185822ca8bd51f3d37aa627826      1
    343 0x9eef3d3f6aae0006036d29ca822879ea85ad5a6f      1
    344 0xa0443253fbd322ce7cf2414b0045e6a8ee83cd4d      1
    345 0xa11fda9d1a162996c1c24801b66a27cb0ccab184      1
    346 0xa1f003bc27a6a77c4899c6b1d10a7ecf3cac29f0      1
    347 0xa309e257db5c325e4b83510fcc950449447e6bda      1
    348 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    349 0xa3e038cfd4708e8896e82f0824e3567bbb5852db      1
    350 0xa40633df77e6065c7545cd4fcd79f0ce2fa42cf1      1
    351 0xa454daff2609603b169e6290fe69667705ed11fb      1
    352 0xa4a1f13f6d71fe8261f7d539bf7b9a0fca2a0cae      1
    353 0xa4c08137e0a0401b0c94c2e7ee1794f258bcb228      1
    354 0xa5eb522c4f57a5c2c679a502e8baf45a0e757732      1
    355 0xa681b8ade97554030760b8d0e7bce39731990ec4      1
    356 0xa7108875badb651de344ba18844758cc29bfa385      1
    357 0xa7259f3138401412926308c0e301b9d4d31657e2      1
    358 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    359 0xa7c131b95e5fe8fd76a4bddcea78f415b7881c3a      1
    360 0xa7c85ce7d129d8bac5b325b54312d6196254f7e8      1
    361 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    362 0xa8b1641a6c0fff08b95062cae59efb20b56045ed      1
    363 0xa98d2867de6b1dff44fe4b355dea098e81d06aeb      1
    364 0xaaadad9760abfad2d88d465a42f85b0fc24133d3      1
    365 0xab598be7413ae9a89bb319e36b8aa8b31cbe6314      1
    366 0xac47c8d0bde102e60d894ae9b5f9ca519af1f455      1
    367 0xac62b7b2c2c51fd548b42adc15be68e8fcbb7b94      1
    368 0xac8588e7afde31d1ad9fb0e4f70ba18a45bcac28      1
    369 0xad36ee68717899d89331073b22f78b633846d389      1
    370 0xad4d39612f84636ccfeb4c056d6bce7e8be703d6      1
    371 0xad6ef986917e1ecc461623e22f5b628d546d1ddd      1
    372 0xad9d6fbba0eec0d77355663468b2fafd44678fde      1
    373 0xadc8aa34acc75258eaeb17242c953f1601c5bdae      1
    374 0xadd93b1acdfa3e865bd1e6495182e9f1ac5cc35b      1
    375 0xae5db210a7383902ceb482ff863575518d9fca25      1
    376 0xae74399237e02458aa42283e7c3ef2929ad4896c      1
    377 0xaef151417248d6949c6ac9145fbdbdffcd3eeb46      1
    378 0xaf8fa5568931769e99ba860e78baee7f9522e76c      1
    379 0xb0cb1e862437b98df874e4095a67d423e7d656ec      1
    380 0xb198301bc1a101df202ae35c65dfc773f38c6c85      1
    381 0xb24b94cea4e8aa1a0964d6bc5f8ea516d500ea43      1
    382 0xb26376ac84799261b61f20452a6c42b8f54fd143      1
    383 0xb37d9262cd3c4346653bc7c0c6b6d9c15e9669d4      1
    384 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    385 0xb44c0b07b9cc8e86faa31cd41e0736714745a705      1
    386 0xb6da08cd77da865e99bba2868aac74ea2cd58162      1
    387 0xb70d0906ab07ae1fb9283fefcf3df7d499c0fffc      1
    388 0xb72edf2669f2b05571ae4ee0e045d5927982b1a9      1
    389 0xb758e98c2c99883353d2ab34c4d7c550e4871aa9      1
    390 0xb7fb1a8d215d9bfd2a825db4b2dba932c8dd5c33      1
    391 0xb812b11e5bfbeda4f33da7d96ecb9bcaf8276d71      1
    392 0xb83daeabc2189d72bbf0e8a8a7dd77676e0051ef      1
    393 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    394 0xb8e532c94fe73967b3a3ef67cabacd5421685450      1
    395 0xb9780000151cae6a9bdda030d90af37637182b97      1
    396 0xbb04b1b77be30e0a534dd5aa590a48d1d8f7398e      1
    397 0xbb34666407e47f87a44e4540ee765909506cb105      1
    398 0xbd56aa5c1db55fe445decfe2e069416e9bf23463      1
    399 0xbdc136ddddf8392632b3b0d2589bf722183bd877      1
    400 0xbe5deffeb557d9f0f224d7a9266d1cd5d2ce072a      1
    401 0xbeb08de7778139fa4e0f1f2b8b0da88d997a1636      1
    402 0xbf91459018a2526c745979466b09dbd1d239a41d      1
    403 0xbfc016652a6708b20ae850ee92d2ea23cca5f31a      1
    404 0xbfdd6d498a2479e4218298848db66942fbdb77ab      1
    405 0xbff9a8aafa701117020dc56b85461c477d24eb5a      1
    406 0xc013122ef9df0e92c7654878908c47f63a77c247      1
    407 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    408 0xc040155a8c65621befd26c9e4aa0e4bb9d0ef8a2      1
    409 0xc049085709fc6581152c1e1edeea53fae58fede1      1
    410 0xc0efdd2b7616b1a469d5fc05dcc90f29647f03b2      1
    411 0xc2758a478054263a46f040717490a41866eeeea0      1
    412 0xc37ad22e8ca0184d630d150fcdfd8abd8f0230a2      1
    413 0xc38f5aac5d5bc883f66c62ada1a9b9beafa1407b      1
    414 0xc3d88d230778c1edd6884aaaca3499fd912ecd0d      1
    415 0xc3fc90508d2d3f8c087acb95bf7760f8b79ad6c2      1
    416 0xc4e4984e243f2b833e31513ed4a5025922922d61      1
    417 0xc61cc3107ad698103ace17beb39b1ac139646209      1
    418 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
    419 0xc6a4b071946af8b61aa360f964ba7ba6f62dac43      1
    420 0xc707ed9405ce3d6d99a35a37936bd42b8714c03f      1
    421 0xc7a964d9c711553fb534d113e52948b813730396      1
    422 0xc97663c7dad3515520a6d820d185ee33d3e25c1b      1
    423 0xc991fd20ffd2ee620a73ea7dadd051fe86d39b15      1
    424 0xcaa95ef5bc0f724a76bd4ba66ddeb6d50da4d116      1
    425 0xcaabeef524a933db6e3aca48ed2c4de6d129eb54      1
    426 0xcae44f87d9a8fd6afc20ccae7cdc0011d38ade51      1
    427 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    428 0xcb3514cc04238eacdc08211e85839c6bc403edf9      1
    429 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    430 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
    431 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    432 0xcd486efddbca4d4ac9504f4272a7148376b36e94      1
    433 0xce03c1d61d1bce391a245cad438fd6fc809fc26f      1
    434 0xcefe23368f82a29089dd6c62179eac2e7413ce35      1
    435 0xd003861bbcb67b3d2a26a9e3890bd64c97bb6931      1
    436 0xd026890579a9b1887820a53929b08baed0868e71      1
    437 0xd079cf96fabba75d12cd7f00ad9e99bd5329947c      1
    438 0xd1818a80dac94fa1db124b9b1a1bb71dc20f228d      1
    439 0xd191441efd409fb3ddf1992b0af2f9955d447c97      1
    440 0xd1be411dff67225dc0e691d036e16e2efd72c33e      1
    441 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    442 0xd4e5d6defc141ab74ee65461677087b43f9460a5      1
    443 0xd5363770753094345957130c7790859b2e133682      1
    444 0xd57721b29f2a17ab6a0635210ce05dbbecf5cbf4      1
    445 0xd6deab61aa40c18508a4454e6cb81ce33560a206      1
    446 0xd70a3883f0b851c90cf4662018006ce9aa50fba2      1
    447 0xd7fb200ab4924c252633d1a2b1dc7226ca06c2b6      1
    448 0xd83a5e7bd6eddf35431e0b2aa2680449541e7444      1
    449 0xd91babefdb88393cdfdde2b45df67c985bf3c82c      1
    450 0xd9ef3059f4cb6176d5912c836cf7691c7c3f4fe9      1
    451 0xda57e8cf7f7ba151ec6d41a874b180c28b74af4b      1
    452 0xda602b731e976b109ecd9f24623c03352a0e975e      1
    453 0xda8fc49d69e7d38157a13c79b247c15a37544e95      1
    454 0xdb778fb284f2c25969768fbdb71c2a620ba03005      1
    455 0xdb8470c9aa9d85a503ae3c35b8fa73640fe8cac5      1
    456 0xdce62b21a8b1a9b39dd3ded27c876d416cc91b3e      1
    457 0xde08b96895c99f9983c631f7a9b7d504bebeeb94      1
    458 0xde1b6dd59a147a5cbd584f5ec2e0954cfecd2816      1
    459 0xdeae38389a9ec57c8810855aee8a6927e5b93054      1
    460 0xdf09092bae5c265e404e0a8ce01ebf341481f531      1
    461 0xe0519f722ccf626a93b1275e06f58855b8f9e75d      1
    462 0xe085edf3bed5fdda37fbbc8655122d71563c3bf6      1
    463 0xe1d15b137cfec76e4dc92378eb7f12e1567b62da      1
    464 0xe20b6825862caddee28becbe7cc1ca666cd6dddb      1
    465 0xe2a51d3b9e20389c1347316a132237ca4c76eb50      1
    466 0xe2b1081dc27703f36b444665254b0bda0ee9ed27      1
    467 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    468 0xe3738a4b41e430b1c66f643bccb3f6a06711c6a5      1
    469 0xe39a4b4ee4164254b53eaa508024398e3d01af01      1
    470 0xe3fc4cd9ea032433a31c3719977bac2724d3815a      1
    471 0xe42f18f3d6bbf5bdcdc5064ad0256f24365b01f1      1
    472 0xe434f59930e9cd2dfd2235ea86c9bfdb44f5bc0f      1
    473 0xe440b9fa39d6ed3077795a87eadb8f5f43e8ce88      1
    474 0xe495c36e756ba677d5ae8fb868f8c8a41cc51611      1
    475 0xe4d310c71ea8fec7b9d8dc4ba94eb83e86f08fff      1
    476 0xe5e508c953114f8688b9d6072cb1196cf6487006      1
    477 0xe61c11e9bd86283d77850ff19b5aadbc0dda08c3      1
    478 0xe66aeeafb30c8386773fe5de75a3df329b77986b      1
    479 0xe69c1dc615f6caa50faf7d5831fe09c30a614219      1
    480 0xe6b4c00593330f610042276f7588d1134a15253f      1
    481 0xe72fc299afa973af175ad1c6fab5f46bad7c59b6      1
    482 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    483 0xe7677698617c6a3d4676eb787c1bc10081d5b8d3      1
    484 0xe7fa2f70f9dd91fb9aa5ed659dbec809d06a56d3      1
    485 0xe7fefabced357905e31a456cdc02dff348cd0fa4      1
    486 0xe88abbd906389ddb75889b438a7b642073cedfc9      1
    487 0xe8ed9d3d5ac98b6ff508085904fd355b98080b6f      1
    488 0xe9ad6041bfd863415fd483bc2471db2dfd065a0b      1
    489 0xea2a3f3dc3e40ca326c1047b0cb567e91465c512      1
    490 0xeadcf175cc71ef23cb3a039d67b181688fe63564      1
    491 0xeb6b623da4d6a44615f36b9877dd68b096707cf8      1
    492 0xeba5c4630c35e5c85f9b6d63c50227b5ed93be1f      1
    493 0xebd0e5f6c2655fbc9c1e7800677203cbb74eba91      1
    494 0xebdf2eeba978a96f30f23af3c4bd296996a39b3b      1
    495 0xebe86d665bf7311680fd4613706a2837e6cab8e8      1
    496 0xec4762fb859a53c15d1689c08fd5e48c5c1c47fa      1
    497 0xec7100abdbcf922f975148c6516bc95696ca0ef6      1
    498 0xec92270d4d03376bb5abfd0407062da9e05aec1c      1
    499 0xec9bbd1fa539df97b7117c6e26bc0da2968a157b      1
    500 0xed48ae5ad03fdbadcc70cdf25bf97c26685536c2      1
    501 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    502 0xeec7990c2952017b75e1b508dc3929fe4036ecc3      1
    503 0xeee4ebaa421ed7aa32b925bbe2fe7506e33e7cd4      1
    504 0xf002fcaf8a060808ed39d04e6ff92425080a71d7      1
    505 0xf01994fb823604ad2ab5e3a6bb200bb5ed5665b4      1
    506 0xf16e381dbdf25a5fa30f656f88a2f1366159ba9e      1
    507 0xf2439241881964006369c0e2377d45f3740f48a0      1
    508 0xf248a71dc0b1bbd4ffb6093207c78a9eecd1c2ca      1
    509 0xf3d42cceeaedd6c5ef2eef0e64aaa335d07faa59      1
    510 0xf425af80084107ee96cad6f4f5884af597ac4da8      1
    511 0xf50bdeafecca3a679d26ae4c2dec953e032a6fda      1
    512 0xf5b0dfe3167726cd164d74774b2ccda79626ec92      1
    513 0xf64addc6a0f8ee7691bec27cf0ff800e2d6a4156      1
    514 0xf67f4c08be1d8feae815cbc70bf18966c19c5ade      1
    515 0xf6991ada5e45e964b7b38dd40165522530cb6f08      1
    516 0xf6d47763f157f42e8bd711a3b41510267eaf4ba1      1
    517 0xf6ec8fa73d67008e9a504c2753a9cbabb9f038ba      1
    518 0xf7356eac86640c188eb5116b593e089f3e1f2131      1
    519 0xf7b05bcc0d9f6aa171cadaa6dd08cb9c0ae99ed3      1
    520 0xf7b6409b12a540947688f054dbf5b629fb3fc7d8      1
    521 0xf7bb69ced74dea46131c4202c784810d0957930c      1
    522 0xf7f15442f24a976b2e6254a547b50a11e0a58097      1
    523 0xf9433b43154d9be60edcfa8acbd3d136c67added      1
    524 0xf9d6fce6cce28c0cd62c5caf045f4f6233888989      1
    525 0xf9f582f9121b143dafad76d1569e254b399b1cc9      1
    526 0xfa7e88c1b20b84a329d90b4cd84ac3ba14aa4fe3      1
    527 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    528 0xfc0344564a36d3f572beca9cdaf67ca123bd4b45      1
    529 0xfc1ede2875b811df16e4582cbdcc1bfbdc8f77bf      1
    530 0xfc39dd253b7f9d4f8b5cb9d6a2c911080f504327      1
    531 0xfc74fadac87796a9d9d54d4df37f01d5797d72c0      1
    532 0xfdd3980797820a22e1eda8460c2dec333a9c3ae2      1
    533 0xfdf82b1c1a9065cd0e42a6c67826ce93f8e067f4      1
    534 0xfe5a902288844b856f36dea30cdaf82de483b2b2      1
    535 0xfffba2fc380b0ad389652c75383a6193556dd7e4      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow Memes Phase 2 (Remaining from Top 500)

``` r
c(allow_memes_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 179 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      2 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
      3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      4 0x039649f7c2f548692184da3fedf316f58e8356c0      1
      5 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      6 0x04294157cfba9ff0892f48f8345ea3539995f449      1
      7 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
      8 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
      9 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
     10 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     11 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     12 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     13 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     14 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     15 0x0e81a8a496f9cb7e1d29382e91f06a6f7416cb9a      1
     16 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     17 0x111818a51c4177e8980566beea68fe334be7b76a      1
     18 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     19 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     20 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     21 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     22 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     23 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     24 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
     25 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     26 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     27 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
     28 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     29 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     30 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     31 0x2d8b81cde837334beb89a9c0a88179ad234770be      1
     32 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     33 0x30eedd4ea7e1959fc9d6879cd26f1d8baabbc5f6      1
     34 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     35 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     36 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     37 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     38 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
     39 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
     40 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
     41 0x39e6d2b65fcfa0b3b2d74973b9eb67b6d68990bd      1
     42 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
     43 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     44 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     45 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     46 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     47 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     48 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
     49 0x417c269b82387ab605bdfbd312d91baf03dc8516      1
     50 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
     51 0x431181dae361813567f35ee2abac73291820fcc9      1
     52 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     53 0x45360f55024132b3110166e1b327170daa2cc299      1
     54 0x46abfa031be839b1599513887a27a403e8d6598d      1
     55 0x488e5685b38d9412cdadae46feed3e863f57ca5b      1
     56 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
     57 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
     58 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
     59 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
     60 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
     61 0x52690f90740621f89f58521433e9b0921d626708      1
     62 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
     63 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
     64 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
     65 0x5f656037e30a003862cf878db24ab5f537177fd9      1
     66 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
     67 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
     68 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
     69 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
     70 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
     71 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
     72 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
     73 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
     74 0x6d35392121a6687fe19d6159601b17517f73d0ea      1
     75 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
     76 0x74d50ea9f8cf5652e75102c3fa049c01534dd6c4      1
     77 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
     78 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
     79 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
     80 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
     81 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
     82 0x7c84aab9f3eb0897191d95afb2aeb56a63973532      1
     83 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
     84 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
     85 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
     86 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
     87 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
     88 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
     89 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
     90 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
     91 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
     92 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
     93 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
     94 0x8874174a2366668d54fea6343f71709389563c8a      1
     95 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
     96 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
     97 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
     98 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
     99 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
    100 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    101 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    102 0x97a45305513133c62b2e3a1f2181dfb5ffdbb450      1
    103 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    104 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
    105 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    106 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    107 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
    108 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    109 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    110 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    111 0xa10bb823fb55a9199e4f521d0a88993c4cba7150      1
    112 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    113 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
    114 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    115 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    116 0xa59422ec0634a0263bcdfce5e29dd2df3803447e      1
    117 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    118 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    119 0xa8806ccb79069c976e37ed1950ada0eacef6a289      1
    120 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    121 0xac02b3653a6fd4d507df142a0337951e39c5619b      1
    122 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    123 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    124 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    125 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    126 0xb4627672ee52660a9e453ec541834e04583f3602      1
    127 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    128 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    129 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    130 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    131 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    132 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    133 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    134 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    135 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    136 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    137 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    138 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    139 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    140 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    141 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    142 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    143 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    144 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    145 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    146 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    147 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    148 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    149 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    150 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    151 0xcd69150ece65cf880eaa7b91ca6fbb7d38e97cc3      1
    152 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    153 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    154 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    155 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
    156 0xdc78107155918e230246439e4159fea4c477eae9      1
    157 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    158 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
    159 0xdded73b0bbdb3782a453c9202f54c746fd391068      1
    160 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    161 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    162 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    163 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    164 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    165 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    166 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    167 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    168 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    169 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    170 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    171 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    172 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    173 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    174 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    175 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    176 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    177 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    178 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    179 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1

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
