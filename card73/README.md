
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16725069.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:2393        Length:2393        Min.   :1   Length:2393       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:2393       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16725969 # https://etherscan.io/block/16725969
block_hash <- "0x6f854a82cac7bf86ea59e09a03ef8181b4e56ee66de5435bd4d46ade29d9f64d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4875 

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

airdrop_memes     <- pick(snapshot, contracts=c("memes500"),address_remove=address_remove, address_pick=33,address_max=1)
airdrop_neil      <- pick(snapshot, contracts=c("SuperRare","SuperRare2","Foundation"), address_remove=address_remove,address_max=1)

allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"),address_pick=20, address_max=1)
allow_neil              <- pick(snapshot, contracts=c("Seascapes","VisionsICM","TheUnknown","MysticalWoodland","LightArtAnnual","DANCING"), address_remove=address_remove, address_subtract=airdrop_neil,address_max=1)
allow_memes_1_phase1       <- pick(snapshot, contracts=c("memes500"), address_remove=address_remove, address_subtract=airdrop_memes,address_max=1)
allow_memes_2_phase1       <- pick(snapshot, contracts=c("Memes501_1000"), address_remove=address_remove, address_subtract = c(airdrop_memes, allow_memes_1_phase1),address_pick=300,address_max=1)
allow_memes_3_phase1       <- pick(snapshot, contracts=c("Memes1001_2000"), address_remove=address_remove, address_subtract = c(airdrop_memes, allow_memes_1_phase1, allow_memes_2_phase1),address_pick=300,address_max=1)


allow_gradient_phase2   <- pick(snapshot, contracts=c("gradient"),address_subtract=allow_gradient_phase1, address_max=1)
allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_memes_phase2       <- pick(snapshot, contracts=c("memes500","Memes501_1000"), address_remove=address_remove, address_subtract = c(airdrop_memes, allow_memes_1_phase1, allow_memes_2_phase1, allow_memes_3_phase1),address_max=1)
```

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 33 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x039649f7c2f548692184da3fedf316f58e8356c0      1
     2 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     3 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     4 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     5 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     6 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
     7 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     8 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
     9 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    10 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
    11 0x557c60995797fa7b47be105227a2e46148d85750      1
    12 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
    13 0x74d50ea9f8cf5652e75102c3fa049c01534dd6c4      1
    14 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
    15 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
    16 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    17 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    18 0x8d12c02671848b17c18322027a2578ea7afbb702      1
    19 0x97a45305513133c62b2e3a1f2181dfb5ffdbb450      1
    20 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    21 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
    22 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    23 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    24 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    25 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    26 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    27 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    28 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    29 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    30 0xe7f31a4516dbebc0f443a8d4ac8b22d1a8f096e0      1
    31 0xeac5f9b3cd48123a69fe69ce93a7f58100a56552      1
    32 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    33 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1

## Airdrop Artist

``` r
c(airdrop_neil) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 33 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0571cb3b7b1ee738e189678a22eb3edd342ed5e1      1
     2 0x0dac0d611db5955bbe881cf8d75d4c80271cae83      1
     3 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     4 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     5 0x27458760bf383bd2fa9957ae8aa95e9fd5236654      1
     6 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     7 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     8 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     9 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    10 0x4897d38b0974051d8fa34364e37a5993f4a966a5      1
    11 0x5c2a00d4051455f05829d818dac18e438124e79c      1
    12 0x5f01ef2c293e527b991c4c9d61051dec226c2f7a      1
    13 0x5f22dbd716ec054a9802d1d0632902398d097c36      1
    14 0x5f9d41289ad44d17e6c51f889276999112e4fffc      1
    15 0x6456f20453a383004f17b921cd770d187fcd30ef      1
    16 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    17 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    18 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    19 0x6e2fbc139e37b43f4f29cc10b7bede36587f7306      1
    20 0x73254d06f0e28f6602866d128a5f451917988896      1
    21 0x92767f92d180d66c6420d90fbb369fb12b7398ae      1
    22 0x96951beae8a3c191800877cdde67dc12a5881584      1
    23 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    24 0xa40d281de9c15d501ef21ac32f777a5a551fddd6      1
    25 0xb86ea3223fa32f1bd7f7ffe9dec2f01d30ec3046      1
    26 0xc385caee082bb0e900bccbbec8bb2fe650369ecb      1
    27 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    28 0xc7502c148b7cf1f9fe9e7a255a77f9c1fd7dccaf      1
    29 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    30 0xd2d2aebe8eb2d53f132366fb98c5a62b72694b78      1
    31 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    32 0xe72c4bea4b5f5caabeaca6cd38894f8ede1f2e16      1
    33 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     4 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
     5 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     6 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
     7 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
     8 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
     9 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    10 0x69e68074f1aada957edd39c5eae0069973343f30      1
    11 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    12 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    13 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    14 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    15 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    16 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    17 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    18 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    19 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    20 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1

## Allow Artist

``` r
c(allow_neil) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 172 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0187c9a182736ba18b44ee8134ee438374cf87dc      1
      2 0x0401343c50ff963a7b02b20b31fa9b0b159354d4      1
      3 0x041e1fe38d7de3a10ed9291c44435ae29fb0d2a8      1
      4 0x070d9f2c2af6b930c08b09a3f8d16aba98efd1a2      1
      5 0x08e0a3e752d1338c0f02b986b9504f1009a962ca      1
      6 0x0a712c5fa7aaee99a6679ee9bbabd62ea5c1caaf      1
      7 0x0b4f222cb69554ee96bae54abbd5583d40539266      1
      8 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
      9 0x1155edeb324ffdcc8ab3680b6b04130069bb1967      1
     10 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     11 0x12b520c229886495fb88da799a285194c23ddbb5      1
     12 0x13b5de1a572e2f2fc5c3d9aaae88583e510f6af6      1
     13 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     14 0x17523002d66dbdda6677646d256a5b442107251c      1
     15 0x18e6008425741670d162a5598b93bc216756a6d0      1
     16 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     17 0x1a4b7409e020140443a02ef26858a7e53ac7d652      1
     18 0x1b052edc4df735f0a5e62c1335c94667e7fdc55e      1
     19 0x1b1e8ea518f5f23253c4543c2f38e668aad74c07      1
     20 0x1f623ee324ca178941545f01bfccfea38cda2072      1
     21 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     22 0x20c8d72bf36a74799fdf8d58986195877f328d6e      1
     23 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
     24 0x24fbadccd6684106e24065694ac87b0e98819235      1
     25 0x2506cd8c7bee35ebe54670bd28eb00692eacd426      1
     26 0x27ad1a687c85acd4245516e9a42325bf91bbaf00      1
     27 0x28f728356fd5cb052c6de19842aa2c713e8074ed      1
     28 0x29dc39ee6ee4c9d40b9efd5bafb9a1d41e7d0d37      1
     29 0x2a76f7df64889a1f20f5b6aa87ebffa9a38ab925      1
     30 0x2ae70d934621f014112b47f7356741b765a7fd93      1
     31 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     32 0x2f63885741bcda3e66a2f5fadeeb0ce7b49c25c7      1
     33 0x3113ab5c691ecdac25c07bbf2bf80eb005e4088c      1
     34 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
     35 0x33a1a5fb1b4a3652c446b50bf4d15204c4d1be24      1
     36 0x36af69fa35c61841349f5bd3f09efe94e5cebbb9      1
     37 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
     38 0x388160a99390392278afdba240046b8b5e73f77b      1
     39 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     40 0x3b93799ff5cbc717e6d0924514e09e694a4e23d7      1
     41 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
     42 0x3cc274fa4c5ea62ea8581123d552fadb71f9f2ec      1
     43 0x3fb160a681806acc91aed14c284c09a96ebc9dfc      1
     44 0x40e6bb0ddf95d710f4e32461f136ada7baf73aa0      1
     45 0x4202c5aa18c934b96bc4aedb3da4593c44076618      1
     46 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
     47 0x42b46951ae492fde820016ad6432c1360a227dd1      1
     48 0x43bbfa2821b4dbc2528796d8ec48e46541ff8a6f      1
     49 0x44dc0cc467ed79d1ea431a1055c57c77faa721c5      1
     50 0x452fcd9f73a5f09f6b6be34b708e4c2b7d8fc46b      1
     51 0x47015e64f50ba8516c076cd211f92c5c6a4b352f      1
     52 0x47279fc734466ee8acfa50aed553ff79770c8779      1
     53 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
     54 0x51c780b8e7b8b318fcd1b68f663d82cccd27769c      1
     55 0x562257821731283b842cf689df9941ddbec3a2a5      1
     56 0x56616a6dac6252fdbbacc8ef5e739f4b26119f92      1
     57 0x59a2add327fd2229a644721e68214949d4a2a490      1
     58 0x5a6c8df7d9acea10512420107765adc4209d3326      1
     59 0x5b2e5abc57721728d63928a14ca58b7120ff74a8      1
     60 0x5eb659bcfdd85692761f8ef476f67f080fe198d0      1
     61 0x603428de2f58af4a0ab4e53930cc9c073c2e7d1f      1
     62 0x612181772106aedc4114c939a36a2795ce101ae9      1
     63 0x64a8dda827deb895a36667d01063f2ab840e46b2      1
     64 0x65423fe245198e8d6746a3aab09ef6c2d486a887      1
     65 0x675a3067165fd4a36200009fd5854c5e3b55f670      1
     66 0x698f0664bdc21afedd384092f185fd8d48f02bd7      1
     67 0x6a6d2019ed5079f07e84d30b4d702a601eaaa5f6      1
     68 0x6e478cb3fe4fb88e878ad25b883a14f7231f9bcd      1
     69 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
     70 0x725427aaea450747aa33740e44f7ff6b8c7c693b      1
     71 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
     72 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
     73 0x75e5052d53dfef2dd831814b8da723900fa1e337      1
     74 0x7635daa2c1f2f9d4be9b8d91ebd25001f74b78a4      1
     75 0x78589dad2a38a92b6913c5e817c7e47104da6351      1
     76 0x78ebe56bc138069557c89af35eb29023ff31ae2c      1
     77 0x79b834f7f6f3ce94aaf63fc4a2b2f722e69de40a      1
     78 0x7a926e039b30b9ca5a0499e6a8b1f7fe2c30aef8      1
     79 0x7bfad1dd12a8010b66db7af2606793a3a7938d6b      1
     80 0x7e330570bd914b3d70c9502a702a3d4c24ce51bd      1
     81 0x7f26d2edbe7ed3bc6e86f6981ef35fb421376dbd      1
     82 0x812010bbe6652497d33c7a1fd986f629769ea9cc      1
     83 0x8497277c9339170a0420e86da8352e0c084624cd      1
     84 0x84cd67d2bcb28694b7b02ccb9736bc9547181588      1
     85 0x895bef95023f591ebd1a9e8f53bed2b73702e4d1      1
     86 0x8d3f70206c4547f0844b9d2a6f3639634ee0bed3      1
     87 0x8d4885f5677806d77ada7923298e2222e6d01da3      1
     88 0x8e1311bee91e4ce25f4f2ea2e998f946e57ff094      1
     89 0x91364516d3cad16e1666261dbdbb39c881dbe9ee      1
     90 0x945fc81c9edc66609f54cd19cdb4500713f8a825      1
     91 0x953044e85efcf894bce94a9b942a2ee0760beffb      1
     92 0x9580a1de630c1c92b6ee0f08d0d9bbae3060b9e7      1
     93 0x98949eaa1189181b765e5e6b8eb854484c8ea059      1
     94 0x9b2942a46e23d9347f53c878531787bb34bdf807      1
     95 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
     96 0x9c637f5f727825932bc332ad10ec3b41c200c23f      1
     97 0x9f9fccb852eeefddcdf10989461531efcb72a4c4      1
     98 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
     99 0xa1e8c429c49b3c628fcef76b8d59495cf2c8bbe6      1
    100 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    101 0xa62da2ea9f5bb03a58174060535ae32131973178      1
    102 0xa7bce13c268c132eafa61633827b872a248cb352      1
    103 0xa7efbb4a06a680f3b65c5861ec387408ceafbec8      1
    104 0xa7faf09d160d777c98beef579087266f6da167c9      1
    105 0xae9934cdb0d403b2247378bf53b597faf7655bb0      1
    106 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    107 0xb24f9a517183b489691850ccdc6a17fac5ae6532      1
    108 0xb2eb4de0b3aa095528dcd60c74671c6f2dcb1253      1
    109 0xb3e37aebf26dc322857f66f5c889118ed6bfadb4      1
    110 0xb3f3658bf332ba6c9c0cc5bc1201caba7ada819b      1
    111 0xb4b732fd4d12149d5ac06c70032c657b976ccd66      1
    112 0xb519095fb7f2438d778f44afb9cdc2f9611d85ed      1
    113 0xb5a4a9677de36140b4330a3c90500a091723da4b      1
    114 0xb6fef67b00d644fbb5e4368aed9f85ec2d134fd5      1
    115 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    116 0xb75f87261a1fac3a86f8a48d55597a622ba3cc48      1
    117 0xb79c26fafaafeb2835ff175d025db1b9deeedf5e      1
    118 0xbaa02edb7cb6dc2865bc2440de9caf6a9e31f23e      1
    119 0xbb4b25d681dfbebc211349804ec5d634f9306122      1
    120 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    121 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    122 0xbe0c0df368687e580fa2c3ad3c9412981f0273fb      1
    123 0xbe314949e2b9d14c27fa6785323f7cfc9250f92a      1
    124 0xbf59e40112157017b69be5734f8c702dcbd73336      1
    125 0xc10fc4f8566f2e89cbfb682e50552c193aa1244d      1
    126 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    127 0xc293859e6818c3d3d2ea39287cd92bf87efd2d12      1
    128 0xc2bc916f748ba52e2765f633ab5d7016e0954e54      1
    129 0xc343b0087fc5b3f0108f4cc96b3b33e52a04987f      1
    130 0xc3cdbc995698254ae6739db0d8e248aafa8d6116      1
    131 0xc687ff3ca253143466c2bfeed744ef314397f9c3      1
    132 0xc7db4c95e3050c73db2da4041a03c24df14fd024      1
    133 0xc8a974a97f6a7f57b6ce09aed5905d5547039f11      1
    134 0xc9a194bb6a3855b14dc4699cabe7b2881c3f6552      1
    135 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    136 0xcb63de5613de3add32a48c6736f34e1c8de5281b      1
    137 0xcb748f312b8e0557587862225697aae325052f7d      1
    138 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    139 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    140 0xd018a7b52ce62d436e2e4d7e9b0175c1a7eeba04      1
    141 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    142 0xd31ae1707fff96401f716fbde73af6687e481631      1
    143 0xd38919a385cf9bbade3249dad91b8d880b42bee0      1
    144 0xd463909c1d6ca7edc919f9fc1256baacdd6dea86      1
    145 0xd601d9337bd714677580fc8907a5fe6636dc2c7c      1
    146 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    147 0xda4ff7b6a1eb3176c347a3ffc56a26ee82fb6893      1
    148 0xdb24f4bdf3f171f8f691cf9d2d4cb4e77675453b      1
    149 0xdb517690de79eec1386c739fa0bf7edb289345e1      1
    150 0xdb90f65394a4579e33de03dacf1d4157fd027f38      1
    151 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    152 0xe06233d6684246ed35cfb07c8970bb8312805602      1
    153 0xe08198a7726a95386117c40c637a3f375345911e      1
    154 0xe126731c09e9388e0375243b91a5a9a3fcc64b8c      1
    155 0xe317eb46da9d27aa3493b03ea0468ffd37ccc2e1      1
    156 0xe6cb101eba87c46db7bb1e27ad2bd73482837a1c      1
    157 0xe72eb31b59f85b19499a0f3b3260011894fa0d65      1
    158 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    159 0xe96ead0ad625122677684448155ba4ae2700814d      1
    160 0xeb8e94fb652a4e18b18ea32946e391ae655c188b      1
    161 0xecf36f8a7d3738632ca3dc6ad4b9f307d0e62a28      1
    162 0xf05155f792819710da259c103d30e4b70178ea9f      1
    163 0xf2a876de875f104ac9941a5621cef57a69074da9      1
    164 0xf2dca9d0652c43784522397c11b19694c73074a6      1
    165 0xf3a388b2139ef70d63a278ca48560bb403b5644c      1
    166 0xf596de113b75db5093afe4f92a70e821630b62f5      1
    167 0xf9d681c3b81aa1d0ecb3fdb4c69ca57714eb63f4      1
    168 0xfad0a4097d64e5950f04f4e96d5609a96eb6ac9f      1
    169 0xfb74eee69be0c2bd8fb42ee67b6b6c4e05cd9ddd      1
    170 0xfd15831377e0b08246629d5ba9a06dd60c438923      1
    171 0xff5fc3640912c289e9ee5a93dd8e8999e2da94c7      1
    172 0xffb8a15dc8796af3e7bec74e528f1bd810b854ed      1

## Allow Memes 1 Phase 1

``` r
c(allow_memes_1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_467_1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 467 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
      2 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      3 0x0216ac50fdb6b46f6b74254b84ecdd1d431d670c      1
      4 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
      5 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      6 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
      7 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      8 0x04294157cfba9ff0892f48f8345ea3539995f449      1
      9 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
     10 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     11 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
     12 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     13 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     14 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
     15 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     16 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     17 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
     18 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     19 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     20 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     21 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     22 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     23 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
     24 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     25 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     26 0x0dd38657d7c8024e7d62bde3d0423ca34769be50      1
     27 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     28 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
     29 0x0e81a8a496f9cb7e1d29382e91f06a6f7416cb9a      1
     30 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
     31 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     32 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     33 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     34 0x111818a51c4177e8980566beea68fe334be7b76a      1
     35 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     36 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     37 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     38 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     39 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     40 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     41 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     42 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     43 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     44 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     45 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     46 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     47 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     48 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     49 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     50 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     51 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
     52 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     53 0x1af369bc1069dd286ff59cd69553550c07e0dd05      1
     54 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     55 0x1c172d05b75178fc669d74407243cc932030f139      1
     56 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
     57 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     58 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     59 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
     60 0x1ef43b94fcb00872ec73d60ff37f5c77df80ee66      1
     61 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     62 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
     63 0x215cbb1b60e2cc1f1e8626cadca386b278aa0dee      1
     64 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     65 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     66 0x220da3fc3b905c968dfb20c81c170bc4dce56603      1
     67 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
     68 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     69 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     70 0x23602ca06e977c86339ffddad74966e824ab691e      1
     71 0x23ae72f8336aca747ef02d596403de56cca489fb      1
     72 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     73 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     74 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     75 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     76 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
     77 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
     78 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     79 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
     80 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     81 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     82 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     83 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     84 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
     85 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
     86 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     87 0x2d8b81cde837334beb89a9c0a88179ad234770be      1
     88 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     89 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     90 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
     91 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
     92 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
     93 0x30eedd4ea7e1959fc9d6879cd26f1d8baabbc5f6      1
     94 0x313c8673ff69dfd7be2a46bc540305d3e2d233b0      1
     95 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     96 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     97 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     98 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
     99 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
    100 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
    101 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
    102 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
    103 0x3876be5be4998adecbfbbad26604a762467e7f42      1
    104 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
    105 0x388160a99390392278afdba240046b8b5e73f77b      1
    106 0x39bba3c54a3412127bdfb0f218efea52b5d28300      1
    107 0x39e6d2b65fcfa0b3b2d74973b9eb67b6d68990bd      1
    108 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
    109 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
    110 0x3bf55d8aad7ca0c5cfe1f697d7783437b9e034fb      1
    111 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    112 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
    113 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
    114 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
    115 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    116 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
    117 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
    118 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
    119 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
    120 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
    121 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
    122 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
    123 0x417c269b82387ab605bdfbd312d91baf03dc8516      1
    124 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    125 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
    126 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
    127 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    128 0x431181dae361813567f35ee2abac73291820fcc9      1
    129 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    130 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
    131 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
    132 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
    133 0x46abfa031be839b1599513887a27a403e8d6598d      1
    134 0x47a79ee280b4a6a8d68a3ba16e4082a765a2b20f      1
    135 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
    136 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
    137 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
    138 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    139 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
    140 0x48789de67a2c509f3676ebc70def28fddf64d31f      1
    141 0x488e5685b38d9412cdadae46feed3e863f57ca5b      1
    142 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
    143 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
    144 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    145 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    146 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
    147 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
    148 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
    149 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
    150 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
    151 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
    152 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    153 0x52349a2c4ea4e4b94ada3d75c9d3a318c024706f      1
    154 0x52690f90740621f89f58521433e9b0921d626708      1
    155 0x527bb834cc5c8ff730c673880e51372282b06e14      1
    156 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    157 0x53b2da71db2e266882a465d9aeeaae3e8beeb9ee      1
    158 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
    159 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
    160 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
    161 0x54913cc8ea17731d62589039dd0152f306473843      1
    162 0x557bd9e829f35f1d853c2c85341458892753fa1f      1
    163 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    164 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    165 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
    166 0x58059014c5952c04370bcb88a2e0503e9eafb209      1
    167 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    168 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
    169 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
    170 0x5d25087405105bab12624c73488ec186066a6376      1
    171 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    172 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    173 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
    174 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
    175 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
    176 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    177 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
    178 0x614b89f072ea263a9387460963142e73548fbaf1      1
    179 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
    180 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    181 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
    182 0x64f02c24bb510e27b5eaec705eaa840ff7b4df15      1
    183 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    184 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
    185 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    186 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    187 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    188 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    189 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    190 0x69cd43dd4ecf3a076b1b9d2cfd41589987129bc0      1
    191 0x69e68074f1aada957edd39c5eae0069973343f30      1
    192 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
    193 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
    194 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    195 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
    196 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
    197 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    198 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    199 0x6d35392121a6687fe19d6159601b17517f73d0ea      1
    200 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    201 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
    202 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    203 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
    204 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
    205 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
    206 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    207 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    208 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    209 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    210 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    211 0x7729cfd6841ec449c20fa7d3970d2e0d035c856c      1
    212 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    213 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    214 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    215 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
    216 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    217 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    218 0x7c84aab9f3eb0897191d95afb2aeb56a63973532      1
    219 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
    220 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    221 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
    222 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
    223 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    224 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
    225 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    226 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
    227 0x808421753a181e96812796b7ab43d3f356cc5a77      1
    228 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
    229 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    230 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
    231 0x82139687faae8a29851902783e02e699de0e0846      1
    232 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
    233 0x843708d85621273f3bbc643b348da3a60d5b0334      1
    234 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    235 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    236 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
    237 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
    238 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    239 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
    240 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
    241 0x8856dcf8d1f9be083a25251b0149ad74f971b806      1
    242 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    243 0x8874174a2366668d54fea6343f71709389563c8a      1
    244 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
    245 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    246 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    247 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
    248 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    249 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    250 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
    251 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    252 0x8d5e59e11838cff72af5fb0681d96a9136ad0604      1
    253 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
    254 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    255 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
    256 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    257 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    258 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    259 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    260 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    261 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    262 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
    263 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
    264 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    265 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    266 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    267 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    268 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    269 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
    270 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
    271 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    272 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    273 0x9e1e3857fb2484379858b9daf230379015a7a100      1
    274 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    275 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
    276 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
    277 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    278 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    279 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    280 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
    281 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    282 0x9feba45425c51b03890604d6ad51f4ee7c9d4866      1
    283 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    284 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    285 0xa10bb823fb55a9199e4f521d0a88993c4cba7150      1
    286 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    287 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    288 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
    289 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    290 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    291 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    292 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    293 0xa59422ec0634a0263bcdfce5e29dd2df3803447e      1
    294 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    295 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    296 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    297 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    298 0xa749f12e124790d45e62bcd3f7bf1d2218f2f21e      1
    299 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
    300 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    301 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    302 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    303 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    304 0xa97204eb6b0f3e58e8acff48a2317511e7628949      1
    305 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    306 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    307 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    308 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    309 0xac02b3653a6fd4d507df142a0337951e39c5619b      1
    310 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    311 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    312 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    313 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    314 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    315 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    316 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    317 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    318 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    319 0xb1c11a879c47debc22e3816e7b727fc8bbe3c8ac      1
    320 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    321 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    322 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    323 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
    324 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    325 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    326 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    327 0xb4627672ee52660a9e453ec541834e04583f3602      1
    328 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    329 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    330 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    331 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    332 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    333 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    334 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    335 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    336 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    337 0xb8b8ce5e3d0ad5c0169e130c64048b6d3cfcd343      1
    338 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    339 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    340 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    341 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    342 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    343 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    344 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    345 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
    346 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    347 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    348 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    349 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    350 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    351 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    352 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    353 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    354 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    355 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    356 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    357 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    358 0xc522289168311a765cf17c067f0118578c99cf08      1
    359 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    360 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    361 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    362 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    363 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    364 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    365 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    366 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    367 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    368 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    369 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    370 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    371 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    372 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    373 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    374 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    375 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    376 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    377 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    378 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    379 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    380 0xcd69150ece65cf880eaa7b91ca6fbb7d38e97cc3      1
    381 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    382 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    383 0xd0b2d2d86a45bf2f13a3af83df6e7b5fe98050f9      1
    384 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    385 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    386 0xd1dd467f1630be10d67205d6c816429f8ee49124      1
    387 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
    388 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    389 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
    390 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    391 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    392 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    393 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    394 0xd4e791d799b8873d86ac06a5fedf2f1a75f639be      1
    395 0xd5ec003289265705727b622f1700fe814e54ca67      1
    396 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
    397 0xd7473ef09ae9c072f2ece3fe7ce64e670eeff283      1
    398 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    399 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    400 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    401 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    402 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
    403 0xdc78107155918e230246439e4159fea4c477eae9      1
    404 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    405 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    406 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
    407 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    408 0xdded73b0bbdb3782a453c9202f54c746fd391068      1
    409 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    410 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    411 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    412 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    413 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    414 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    415 0xe25b24cebed3055236e369570a437a99e1d32602      1
    416 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    417 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
    418 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    419 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    420 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    421 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    422 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    423 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    424 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    425 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    426 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    427 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    428 0xecea65d22233e7beed752a06c46cefc6c90a1d7f      1
    429 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    430 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    431 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    432 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    433 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    434 0xeeb9148a32b45672e16591ee18ceabc773b099b5      1
    435 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    436 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    437 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1
    438 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    439 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    440 0xf12e159643edeeba920518cc614820ab5726335e      1
    441 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    442 0xf1f476f144df01480297dca47efca565e8b0c9f1      1
    443 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    444 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    445 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    446 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    447 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
    448 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    449 0xf6109779752d1fdb8b6063a158bed1b02000b700      1
    450 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    451 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    452 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    453 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    454 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    455 0xf81489f074a9f70c294164e07692559269f3defc      1
    456 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    457 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    458 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    459 0xfc5ef50b9d7a080cd620f404efdfa287af9a3ac3      1
    460 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    461 0xfe7ace0f186a54c0be46f992dd3072e0053a1010      1
    462 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    463 0xfeea8258077cc06444679958185f4198dd4cd324      1
    464 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1
    465 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1
    466 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1
    467 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

## Allow Memes 2 Phase 1

``` r
c(allow_memes_2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes300_2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x018305f64f71290f16ef245dd063a7470fde44ba      1
      3 0x01f4ab9ccf822e74cd514b1fc16068e749d37b1c      1
      4 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
      5 0x0573383fcc26eb066010d28d96ab02f7c5cde67b      1
      6 0x05eec49e27704acf5c5ce31ecfcde4e382d78ba0      1
      7 0x06fa75bfaa972845e7d78a6d928357f15a341d6e      1
      8 0x07219c7136f49847a6be9fd1ce27f8afd6a074db      1
      9 0x078cc4caa131eb9d6d3e31847946d5f37fd3f58b      1
     10 0x0c28d263fccf3ed25a48ddcf6561dd9cccd791b7      1
     11 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     12 0x0cb5b9eeed8d51ae57e70e46b2a3596b94ece6c7      1
     13 0x0ce390f18af702cca546297845a4a51d102123cf      1
     14 0x0d08c74268c1260d9b50175c41738e1a45819700      1
     15 0x0d69a096b2c66310309f0ead1d6c97c4dfe87086      1
     16 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     17 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     18 0x0f9c4213c040a39db2ba6f833472f739e61710b4      1
     19 0x111b863c2f7d1833d8f53830647c260169e99626      1
     20 0x112d6fb9c17f75d82d5c5d50efd03ea6af12191e      1
     21 0x1187ec27eb94e65717af65f3ae8c326bd8bb47c1      1
     22 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     23 0x12c3d9510bba6fa40c55d4f30e1be25f5f953e22      1
     24 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     25 0x13bfd26cf7d981aae5f8d4d88af4b34b4b1d4da4      1
     26 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     27 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
     28 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
     29 0x16b5ec9e34bd0a916822d6785b1c600ac911d6dd      1
     30 0x177a8340bddc2041cc362aabf543dee4e394e3e3      1
     31 0x17d6df353c4062b6097ff8c872ff6ce31381988e      1
     32 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     33 0x197fe3ee16556577180f6578050802106e8bc446      1
     34 0x19e3775d47d63e1802577aec70189e7e3d6ac17b      1
     35 0x1be1eac561b34c9b5e45eba9c3fb51fc5fc2796e      1
     36 0x1d522bd33cdd1f60be71b0b7b2efe4e9f20a5263      1
     37 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
     38 0x1ee5106b2233169b84dad2acdbf498b29c3c7d15      1
     39 0x1eec2d7e15d24c053658c2c466bbc59850a6fa22      1
     40 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
     41 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
     42 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     43 0x215fc4d12967ddc2432a7b8936f2849740edb7bd      1
     44 0x21804e35a54aa4820e6cd409d70926a63dba3e45      1
     45 0x21f023839e7b6fee67d33e4548791fa388564a02      1
     46 0x231595e3673a10e846803194f4982e1cf3389161      1
     47 0x235ef6f6bcc455bb284ebefca68754c030bdc1ad      1
     48 0x24fbadccd6684106e24065694ac87b0e98819235      1
     49 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
     50 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
     51 0x27bb5366ef655819337d6ffd29a55905608c853b      1
     52 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     53 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
     54 0x29c02a5e462e187ec7e1c7874d012633f12c89d0      1
     55 0x2a3c05f993d34bab6fa50c72f2465777d1c5d118      1
     56 0x2adfc86a4e073169ac5f8c850a9e80c90383f3f8      1
     57 0x2b919bffea16d55828091fdb8a63f0678e17b26e      1
     58 0x2c1b5f03dfcbb52493a093e8106d7a378c345097      1
     59 0x2cd00bdf1605046cef0f50420831bf3b7d675882      1
     60 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     61 0x2d646486397bbdd894a9bb62d91fc5bdcb8d9f45      1
     62 0x2db1307f1487c2586473f570ef6832d6873a0053      1
     63 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
     64 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
     65 0x30b33f902981b5e567d3bb1a3cc847b35f626609      1
     66 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     67 0x31bedb3962ab3a1d2a2e070853aa5c4acdb734f4      1
     68 0x32dd9f6885332ccf8633a18cb0c2e25e68fe03d1      1
     69 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     70 0x35f4bbcc8490671edf37877684f5aaadfa4235f7      1
     71 0x36d89738405a8325b7c560cab2dd1b88565effd3      1
     72 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
     73 0x37feeac37afa67d5211937b4fca166f91724ae80      1
     74 0x3839a6c85a38de97abd0ecc092ef3311a7f1311e      1
     75 0x38b2d736e41a273d607b24e888a09473226c46b8      1
     76 0x38d779b6dc61acdf864cd289f3594ad05088df95      1
     77 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     78 0x3a0c596a40bbbb558d86b0f692884aadf1dbf20d      1
     79 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     80 0x3b0b262b187001522c34edcafc378500133ab230      1
     81 0x3d9e77bd53dc0c2a3110732e30d0b969713fa4c2      1
     82 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
     83 0x3fd5ede508fdb0ac9e1051bc894d4c9c04ba288f      1
     84 0x429683282e5a28b81aab1f9126b8d5e4847dfbeb      1
     85 0x438ee34be7c6deefbdf0afc21c0d5375b912e0d8      1
     86 0x44b4d430e7c56e4ca04c5799e561063ccc1f1df2      1
     87 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
     88 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
     89 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
     90 0x45360f55024132b3110166e1b327170daa2cc299      1
     91 0x4748ce48e8979e7ab09b2e8df95a1bec7c65b869      1
     92 0x493485c7b822d077f14bd6484a5c40f2adc91c96      1
     93 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
     94 0x4a15e3d354043fdefbb8fa77a55da211f4058190      1
     95 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     96 0x4b0c1beea2e8a8197fc48b0425a50a7204412989      1
     97 0x4e1829da30cd7ae2f3e9915cb8c9f3f203ac7d83      1
     98 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
     99 0x4fed7d0eb3bf1bf1ba69320962c81b132ad4474f      1
    100 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
    101 0x55adbdf8a6396356de42aad937ea79caa2cab88f      1
    102 0x563a7e710f0e6e81d68b4a2fb6c273f179783a2a      1
    103 0x56543717d994d09d5862ab9c6f99bce964ae664a      1
    104 0x57dec6115e561f3de9fb7429cf14de429713b3d6      1
    105 0x593cadf136c68c720d446461c1bfee600647c6b8      1
    106 0x5a3a1461310884df894b7e973305f690fc5779d0      1
    107 0x5a5936a4552382a900a3669665f11472f6a38a57      1
    108 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
    109 0x5b68b49d886603537f8d87aaaaae39ef801353a4      1
    110 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
    111 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
    112 0x5d20e6c0db887b5c3281edfce5bb7677565e2b8f      1
    113 0x60e5299f49cdbc13d152323105af462071b22c87      1
    114 0x62fc78469063720d0ea442baf2984c20b50f0c05      1
    115 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    116 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    117 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    118 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    119 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    120 0x66b280b5778c35c719209614428caddf00aaa3ce      1
    121 0x66fa06bd24294ab3bf123153e49b9065cb7672e7      1
    122 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
    123 0x6ae613a84a155fd80c8e6f14cb3a1d8958f51b2c      1
    124 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    125 0x6b4fedc18a1e8cc0523e56cc9b45080e1ea00005      1
    126 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
    127 0x6d25c0fa070e625dac76b29bcf0917303cd52e7b      1
    128 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
    129 0x6dd97134b53f40d239842716b2cf0179a11876e9      1
    130 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    131 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
    132 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    133 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    134 0x70e680b9493685f72e76243c09993fca768eedf1      1
    135 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
    136 0x71e22168b702bcff528b8974cd4b723250b67609      1
    137 0x72f315c115408afd9240c5be0946c8ebf7261fb1      1
    138 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    139 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    140 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
    141 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
    142 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
    143 0x764abe778aa96cd04972444a8e1db83df13f7e66      1
    144 0x76d01054ff91afc2d515f7fea9a3e3313e248615      1
    145 0x782ff3f609427680db175365c24c238f89fdd276      1
    146 0x7988dd1dda1f35542a2b8d5f7de575563ebf068e      1
    147 0x79d1dd0770fec45768a285da42c2521603c9b91c      1
    148 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    149 0x7ae4784a907460858231609f565bd9580f609b05      1
    150 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
    151 0x7dcb39fe010a205f16ee3249f04b24d74c4f44f1      1
    152 0x7decf7a31168778f311c57b9a948abaa7321001e      1
    153 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
    154 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    155 0x81590c16b9d1495d133cf881acf5cc0b56730b74      1
    156 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    157 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    158 0x85914d049aa004d037cae65bad275ed4147f172e      1
    159 0x86125855ea134f1da31015663fd3c19a657a8d16      1
    160 0x86c71b7d27d00d7814a763f9e6f16fdb09f7e4df      1
    161 0x86ff945e6dc2470317ca082a01de7d24188671ac      1
    162 0x873dfcbbc61ef7ebadb652adb98df9f69399b70b      1
    163 0x88f038389cbe95a917042bdb0f3afc434c680edc      1
    164 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    165 0x8a0c391ed12dfbb23d4a926a7c7353dcfcca8a47      1
    166 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    167 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    168 0x8c4138dcead987bb50929ecfbeb39ef3e28b4c76      1
    169 0x8c8fffcfc3fc82a8777886e984f9b32ab91b2a5b      1
    170 0x8ca7e4a1f59495107bdc4c7b246691d89fd3a939      1
    171 0x8da03feb7174b4b2a6b323f3dc40d97f3421cd07      1
    172 0x8dd129965c6c770a3b34d9ff5034cd73df5eff8b      1
    173 0x903b1b960ecfb37d994fa0cc537777d8ed766656      1
    174 0x9045ede9bc11bc1148a0743ea518e3f36f62bb23      1
    175 0x912e844820eafe9a6c2df0615dcfea91ff32ce75      1
    176 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
    177 0x957b0cd4e9851537aa7cc84e9c7b22bb68d949d1      1
    178 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
    179 0x98172111480cc81621fd8b12bed2fb5095be11a5      1
    180 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    181 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    182 0x9c37bf5faf485b3db73c95300cdce93410b83792      1
    183 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
    184 0x9cfe0d47672f0d9891dc312d242349d52d8aba8d      1
    185 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
    186 0x9ea50708e47a237182a56c481a4d11dd021b7ec2      1
    187 0x9f432a2964351a7830bd9ea10f65107677d73e04      1
    188 0xa081ae42802e0be86f5a6dd9274adf50c9986a1d      1
    189 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    190 0xa225158ea124f311cd0d50cfeaf3407b9412b077      1
    191 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    192 0xa2b55ffb7ada20a70a1e6e79073f2f4d6623d72c      1
    193 0xa40117a1394fc5cfb3576fe12632381b0b79c0c0      1
    194 0xa62da2ea9f5bb03a58174060535ae32131973178      1
    195 0xa76bda01601ebdd50512c2103a329db572d7a77a      1
    196 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    197 0xaaae5c0a8e05ee5b3824b2e9fe939d5dc3ba3336      1
    198 0xaae8c50d76c76fb8947c9a203103d28b55862977      1
    199 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
    200 0xac1a04679039a1718d3820fbc254ce29269af784      1
    201 0xac9169946d4f6fea622900fa6aa1812d5500bade      1
    202 0xadc9e7a7d129be37bb89d05defe860ea28d4e6fb      1
    203 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    204 0xaf8b04fb70bac8a686aa429fb59428e829564924      1
    205 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    206 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
    207 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    208 0xb33fb83c645ac2a12b138636fe59604312092484      1
    209 0xb391806676cec7fd36dc136dbd4097bde13e5b5d      1
    210 0xb406ebb1a43cdc9c6eb5b392c712329c0b84d546      1
    211 0xb6c189179b204d14fcc0b608bc97243b389b0030      1
    212 0xb8e82b32ea205b0546d0ece1a175ad00e404dfa1      1
    213 0xb90aa714bd30e6f135ec15a0cd2605af1590d184      1
    214 0xb98af149b33dee33c97650e877a497bd6a934d54      1
    215 0xb98be1aba8ae2adef0daa18247273fa06021daae      1
    216 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
    217 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    218 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    219 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    220 0xbe9a677708192ae85e54fb38457b3b4f01c281cc      1
    221 0xbf9d462c9c6880d44001bcb8ca4a38a351d583d3      1
    222 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    223 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
    224 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    225 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
    226 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
    227 0xc33164f7ecc76869fafd44363cd094a22e0c296f      1
    228 0xc4c65d75dd6579671db24990bad1f502d7c96c8c      1
    229 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    230 0xc5ed0799c911bf8147680756825725eb038451c8      1
    231 0xc6bd194286802c5975cf681201e41f21a4e256aa      1
    232 0xc7c6eec4978799e6a53da79b540432820a349d0b      1
    233 0xc8725543112dbfb55d54edcd9c9da2239ca1bb92      1
    234 0xc97a5623578a832354988e7e40869f5207193d53      1
    235 0xc99360f97475dcd02cbaa1940e0e6a2cb67186c8      1
    236 0xc9ceeabc1ac7b96fc45a1c94edaa3b10197cedfa      1
    237 0xcb2e86b62b822faf4f50c3251981695d5058317a      1
    238 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
    239 0xcece625176cead11c8d2dd6149fda2496b0c192d      1
    240 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    241 0xcf6492639384eaf2dfdfb62c22c07928693c852a      1
    242 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    243 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
    244 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
    245 0xd1042637cdd71f99f3dc5877030e67f3789e1c19      1
    246 0xd1380a4e089950ee3a23d818e24ccbbef003a432      1
    247 0xd1b643c63d4dfcf4fae32b58d87843553a64b58e      1
    248 0xd33744da3013927fad387d24f57cfa241735ded9      1
    249 0xd361363a6401870ff8a23b7c77c004aecb8c8b84      1
    250 0xd3641bd03a67af07550b049065a19f84333b4b5b      1
    251 0xd3f5ccd478e59c82ab75f393843015e08892a94d      1
    252 0xd4f358d4a415b4abb6f6deb40b86d7db62562960      1
    253 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    254 0xd8f3d5e9a199a35429870fca8befd159bfe48747      1
    255 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    256 0xda23ab0b2636c3f9806a868278d73aef2439ab60      1
    257 0xdafa9e3dae493f0b9d6872eff4fda0f40d1b7488      1
    258 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    259 0xde24926ce7fc297695a05bebc3e0239540978dc3      1
    260 0xe05adcb63a66e6e590961133694a382936c85d9d      1
    261 0xe28470253f0c9c7afbae7f61795d6b1ca4644b2f      1
    262 0xe2b34bbf669096a794397376fb0587e98eb81016      1
    263 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    264 0xe3f3e433102cb26a5e8c88308c9308061ce6cf3b      1
    265 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
    266 0xe59e088489a85a6de407768deb671c7e493fd799      1
    267 0xe5cd0fc813d020644c496dd964a32eb9ac17e50d      1
    268 0xe5fd5ec4695b93d7e43f5d96657a535e0ff499bd      1
    269 0xe7985c511d1bf0c7668757650b974513628dea7c      1
    270 0xe7de1e998ee34918cabab534282803fce02e3f40      1
    271 0xe831977b52714501b52bada9034021a7cac79709      1
    272 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    273 0xeb54ad6b8c3e2d43d72b6274627747ce5bfecb66      1
    274 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    275 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    276 0xee8fa9b1d96b23af51e93d3fe050258edb606f93      1
    277 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    278 0xeee4ebaa421ed7aa32b925bbe2fe7506e33e7cd4      1
    279 0xefb3da5189a6169a61176d1357579e135a1d1187      1
    280 0xf0038d118400ea9510ea3e93f264b770b81145ea      1
    281 0xf08dbb788c290cdd919e1c124f183988e91e9012      1
    282 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    283 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    284 0xf33f6c5302fee8dcaf6363f6862071092b3d56b0      1
    285 0xf44b63f62c7b6c59a883fdc67bdcd692995b8bbd      1
    286 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    287 0xf54913955c6035a12465e24ba2f162df2a026d9a      1
    288 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    289 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    290 0xf868a2de45bc38844b16c9e63fda5e1de1d54a22      1
    291 0xf86b8b8d3433de1514cb36eff3bd6ff8f707c2a2      1
    292 0xfb3eb8b5af7d9c38a1b6d6f4a4882cd07c85d2dc      1
    293 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    294 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1
    295 0xfc7d068f069e712dd35b439bf6ed9df8f58d07de      1
    296 0xfcdf3c2b2b6ba00e6db59c70c424b27970ff3974      1
    297 0xfeb41f7dc10c98fb5a7995fd9c5de7c83e02dde7      1
    298 0xfec4465b6aeac9c90e552ae7766bdc0ee0d8dbc9      1
    299 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1
    300 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

## Allow Memes 3 Phase 1

``` r
c(allow_memes_3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes300_3.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007fcf5590179336c826a60c80976623c7bbc29      1
      2 0x00af1f05c68de87cef3880012feaa227e1d6280d      1
      3 0x01282b55c6be0b0d724d2a9324feb3c08229f2ca      1
      4 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
      5 0x0262adbcaec7d20a384b1edae24a602adecb6e64      1
      6 0x036302a75c0f7dafe15f83c8a1faec282a74a03b      1
      7 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      8 0x04d0c64d8b303586af5cf6bb37db16bd7b78c43d      1
      9 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
     10 0x05d23f26b6dafc7043fe94fbe85379e6bd0bcedc      1
     11 0x06754d9291ab4f4b67fd5a12d7fb05b766cc2b0a      1
     12 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
     13 0x07e34a9ccef73d5cc36d89d6767e3760658340de      1
     14 0x09e6267d0cb691fe647da5029d28ab0fd62d9325      1
     15 0x0a9bc3b0d19bca983cd9a6d251dc5920e87b223e      1
     16 0x0b983c0502ccb2dc598847b467a1b36c1ae8d8c2      1
     17 0x0c358331732fe7c1a19cf78199c87ac9670c0ab9      1
     18 0x0c9d7de299b6f803279b79e8ae06c6a27e6f3f1d      1
     19 0x0d5668339c3c17aa7f06ba7fe2667536cf15d748      1
     20 0x0d869e49e12820182f74dff1aabef81df4e60960      1
     21 0x0edc79adf90a6f6960932488ae70e8c22db9c208      1
     22 0x0ee7d116e0c89aaefb3c3653cdb71e384766ad11      1
     23 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
     24 0x101a8900e4619a192db5c2caecf4be3881320b75      1
     25 0x107d1198854fbbcb2e1014c3ffab166b2595586a      1
     26 0x11099ac9cc097d0c9759635b8e16c6a91ecc43da      1
     27 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     28 0x11e964bdb0504ff35dee22862da8f72603ab4289      1
     29 0x1228a857fd7ee845f4999f33540f6b9d0988e80d      1
     30 0x1236b147e44366e6b954b344cd9afc72bf71b34e      1
     31 0x123cd3fbcae960bbb0e7f68593b453186065c6d6      1
     32 0x12d0f6e748483771383bbb1a0ba1fcaeb2af298f      1
     33 0x131adc38217160e51c33e3aa61c4cd856179bb10      1
     34 0x13a716dd20763000cbf87f999548c3d67f936d35      1
     35 0x15237b9fc8c244246abee701f07e42185c5111c3      1
     36 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     37 0x19285d3163255c6e5d225dec48d3a5b797f381b7      1
     38 0x1989e01962646dff41d4994a411de9621bba5580      1
     39 0x1bb6dc39ebc757db05557d00b16c28a753a20558      1
     40 0x1c97ba3997c34ba30b6ce2e9cfc3eba0b544d752      1
     41 0x1ce4d8f0362efa9d16eabd90a2e6eb683e7d24e0      1
     42 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
     43 0x1f57b7cf79f16e1a4e54d2bb881851679370be44      1
     44 0x20a2cd3502c0a972927e1699def217289e0c90ae      1
     45 0x20a874282b7d97f4fd1b58c0a831964d321eae12      1
     46 0x212caa051f36024a53ebc7817fd1f93e130d188d      1
     47 0x2227cb63299db1ac748347be2eecd8238efd593d      1
     48 0x2249f5016b1b7a306c5e98765bf94fd0a70d8b45      1
     49 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     50 0x22fc06a488b236753a89caeed39b58046b153069      1
     51 0x244fe02fbcf4db4ad96063b161f00e444fc54011      1
     52 0x26088bd0575c0ac94946fb8dfa0b240c42283e42      1
     53 0x2616d8d5075dc5e878f40497a4e45924674440ba      1
     54 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     55 0x286cd2ff7ad1337baa783c345080e5af9bba0b6e      1
     56 0x2be21793155e0d5b3dd715c348f7e712471e0873      1
     57 0x2cd03d3ce4c5ec075f259ccc85d19706a9090dcc      1
     58 0x2d2052be503780c575629a22aa84990f9e38a7d5      1
     59 0x2d37340a97408a9901bd94f63bf0437beac08ac6      1
     60 0x304fb836fb645c992b773efae33c7d9f272e74bd      1
     61 0x30607f2c34d2e847c41fee6b29c3301082031350      1
     62 0x31d240db67be4962edc1803f8b2b4da4230f62e1      1
     63 0x332ab3f0dd19b601e988024bce17c9795a8c88f7      1
     64 0x340b69daa84271b53a4b31d801ba72d19e6c934a      1
     65 0x345ece8b7fc0e91a066130e455b951aff81cf0b0      1
     66 0x34b045e74b751dd5014653c73f910aea2402005a      1
     67 0x34dea787524a930670862a27bc7234a1897e1bf2      1
     68 0x34fd55a860f1df45bf006a81c2c3136f48254622      1
     69 0x35f7f29f154a28674c8a50d435384744b02cf42c      1
     70 0x3667e767a12f9057ef12dc58764400b34cd88320      1
     71 0x36d4706d9d8a106d2320fe14c9da4a4d13cbf498      1
     72 0x373db7e01ebfb92d9095ae6b8f6e3d24ce6f4d4d      1
     73 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     74 0x37ba996dd7eb738af38af7839a17ad19cad5f155      1
     75 0x37cbc09501e6965c97c0e4984a0a371b5d591dd9      1
     76 0x37dfaa62f0fe5e6ebe990ef1a0722f5279962e37      1
     77 0x39b0b2bb9c5dc3c63a715c16f9115d456a212780      1
     78 0x39d3158a7c9d4f8eff36524b9a2065465995932c      1
     79 0x3bbe5768336be5787d2f6d40f7ae673b4c654ce3      1
     80 0x3cd76a7a90e0f6a7dfe6791a54ec459acaebc5ca      1
     81 0x3d055040d91414fbdc0a9eb0cb4e1bdf222fb1e1      1
     82 0x3d6301815a5b5188e50a90e268179447d1c58b70      1
     83 0x3dc3be0a9278028fde0e3ff65c6154e2e36dee9d      1
     84 0x3de4b60cb19faebf58efea6d4cd99fb5295cf95c      1
     85 0x3e3721d26d5b8612bcd6504696b82401b9951ba6      1
     86 0x3ed17fda58b4ca8b4d425fa5a3b92bb496bc0a55      1
     87 0x3f849f47f5b372d80407e442f360ad7b17f5fac4      1
     88 0x41b68202dc64904e09d2e4f0a94879c741f27db2      1
     89 0x41e9e61a699a85daf12dadebbb7e6e517bee6b54      1
     90 0x42757298759773e1af7199b18f6ad2307dfdcd88      1
     91 0x42f2d682d9cee89a98347768337938ed8d18c266      1
     92 0x4322aa46ffed67809862da613725728e2fb8eae3      1
     93 0x450a6049e00c72c8205424a1f3d752ad06382fb9      1
     94 0x452f438aad8b675232c1fd7ff8e940d72d8a9f45      1
     95 0x45557a43891a053d64da64d846b39b049f767d98      1
     96 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
     97 0x47836f027d427e78c06f24eabfdd1d2089fdfd5a      1
     98 0x4799b18582cbb51cc207ed9716034da309edf47e      1
     99 0x47d278101ee9335c3a3baa14fe184f757229a7b8      1
    100 0x47e054a8fcf3e27a4bed979c33ab65a5883fe437      1
    101 0x4820e3cb7d2eb357f28e6c6592968b90e54e660a      1
    102 0x4839ab5e2e84cc9765e38c818c8345c5517d5d19      1
    103 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
    104 0x4d104b21a4338c935dfaf4e00142355686ed0771      1
    105 0x4d42d679cc113ff293f96e1b820209c661ee32e6      1
    106 0x4d8cb131e2835855e37eb277943f308054f4242f      1
    107 0x4d8eb365bc03225e74bfb90ada1c2ada2b6cb8cf      1
    108 0x4e21d3cec38509cf7a3ff9ec53c32be893e9f2c8      1
    109 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
    110 0x4ea7cc46443cbfd0ae19aec35f08dad83477a52d      1
    111 0x4ec4f6f62f766977951733a018395fa8020112aa      1
    112 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
    113 0x516c3ce95dfe03aeb3658a8f25bbd7b8c5970eaf      1
    114 0x518b870520476e08ee523a285e9255b443883885      1
    115 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
    116 0x52d232170f00607b555d97b943094b4ba866f0f0      1
    117 0x541237a3e049f2ef1105694b491152f450aba4db      1
    118 0x55d4a9e5cde6fbe40991e7570e01899891f6b0d2      1
    119 0x56517e41ef464db3753ecfdd2dbcdd2f045b573c      1
    120 0x565465a0e19309ecb839319453e2c83aa0befdf2      1
    121 0x565f34ec537c1e49d7821d713d392027559fe5f6      1
    122 0x56cd7d3e1d4d25d826a95a0c1f5d172e29fea651      1
    123 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    124 0x58e3d079f9ea4cdc913dccf576361f1a5a6b6571      1
    125 0x59234591ae5246b8dc35b07eedf9e1e93d28c8be      1
    126 0x592e480e0066a51eb981b532e275d6576e5730fd      1
    127 0x5bc928bf0dab1e4a2ddd9e347b0f22e88026d76c      1
    128 0x5bff4631326de92282197696366df57198e38dd3      1
    129 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    130 0x5d3dd154c5e582b1b1ac90ebb0c05744038e32e0      1
    131 0x5d63cbbd633bf02a9d22fcba1cbe1e7926a60851      1
    132 0x5dfa3092be17f441f85a4b3218a7675f0efcf9a3      1
    133 0x6072b510aa765dd2f31975c9aa65dde39fd1a282      1
    134 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    135 0x626d75920e4ecf153fa8efd61d31dd26d5cbf932      1
    136 0x6298e18c627e6fad358f3e4633fa0fd54a88d015      1
    137 0x63a612c0b8dfc559318e39daae6d0c7d09212965      1
    138 0x668df49e79e6a828b27f455297291a8bd2fe0531      1
    139 0x668e961736454a2444adb485340cb7f0844ddd3d      1
    140 0x66a71502515e87809205188932dafb640d18b87a      1
    141 0x67c87c11250a3dd2c459d896d862b0c599db4248      1
    142 0x68cbec6e76a8cc835ae75cce5feff1f36cbaf764      1
    143 0x693fb4c126da8e126687b16c809fc19d2409d522      1
    144 0x6a278927f03422419901b77be4d47700b1f3599c      1
    145 0x6afdf83501af209d2455e49ed9179c209852a701      1
    146 0x6b7a91c6a6832bc3498883ff65268612f67c8119      1
    147 0x6b9bcb0b854d7c7430baa55167d367d28a8dbf1b      1
    148 0x6bb59e15545dc9ab0949410cb3d8402ced7fef98      1
    149 0x6c359ab465ec8f893fb6c4243810f4d8933d71b5      1
    150 0x6cf6972be629d30428331a68d4bbbea8e0dac615      1
    151 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
    152 0x711402390e3d26fd137ebfe72ad3909c6e30926e      1
    153 0x71175572caa47abfb535216decb155ed50567726      1
    154 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    155 0x72a52d423878cce91461e9e2ba4bd3f200a18c41      1
    156 0x730c08bb69604080d525181489541ecc13066b29      1
    157 0x733983a4c7e778ca9f57cac6ee361c4a9b24e4b1      1
    158 0x74acc173f943abe0c54cc6acdbb1307715a796ab      1
    159 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    160 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
    161 0x75d7d7972a62b00ff7ef071741c929f59d185ee6      1
    162 0x76500b6a6c640ae7b695ef049f20d24a2ca476df      1
    163 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    164 0x78130d139ee6c5c1b99a49ac70271fe696dc2b3b      1
    165 0x7855227676cce7592e4024056ba2073998f1ead2      1
    166 0x799c5516c59312f229af008c3e09eacfc37dd5b1      1
    167 0x79b5259dc0fcb44f9751bf9f78f5ef46a7958460      1
    168 0x79dc911ba6f8424a4b6158ea0b24168a8e0e0fe4      1
    169 0x79e561168835c783240a0637320d308897bd0922      1
    170 0x7ab2f82adf8b48852cf49034e7e242b45b15ef44      1
    171 0x7ad225751ed67118e9f54f87e08f64e43ffbef0a      1
    172 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    173 0x7bb5cc37a81623d6556d64c5d89333d3aee2a39b      1
    174 0x7be800978fafd15f6501dfc64b27fc74a68276af      1
    175 0x7d3b2acb1a40f09eba725440e14a0e0e5eab4bf4      1
    176 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    177 0x8037cfedb47d493a391dad76c4f60b8927cb8108      1
    178 0x8181dd699e486493027e4e21bf6d0c7b7c94055e      1
    179 0x81845ec7df4fa5776998053457b8c4fb1e60cf84      1
    180 0x81d0b349529339c553484f324f0b7dc8aaaf3ecf      1
    181 0x8396a631492c60696ba4119c54e0fa616b2ffbb2      1
    182 0x8397e2d5fadaa6e0817e376df5ba1d016a2f804f      1
    183 0x83bda4c6d6238ff9d6bb955344e6849ac1ff21a4      1
    184 0x83f0aa19ef7ad4c79e995dd684e06b5b44d3647c      1
    185 0x843f2e3d685caa87ae089c1b95cec9f43ee65afc      1
    186 0x85532659390f6c2e7500f9a28ba9ca26ccf9519d      1
    187 0x85f89831b2311fa2e9a0b0a6a72d27bc40abf0be      1
    188 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    189 0x86fe9b4a3055994fb38175ef7bf3ecf88d0608d2      1
    190 0x87b2dc356091c794490cbbf661384c7e6343ea61      1
    191 0x884b9565d3a9d7265211f9354170a4f12ee2c4c9      1
    192 0x886e16654fecf66c056960811ecc50fe43701494      1
    193 0x8a3b8dfe66bc420c1da15af38bf1ad3640594538      1
    194 0x8a6adb9e6e8dba6bddae8bdfb17fb4657720c600      1
    195 0x8c005e7e9d802dfb1e48074af7d5797303076ce3      1
    196 0x8ccb07293755004942f4451aeba897db44631061      1
    197 0x8ce2b8f8b37b350feb8236f244ccb8ccb6988b36      1
    198 0x8fe89aa22200d1306aed5dad7dbad89c8faf8e26      1
    199 0x918fae5da5fe44f4a37094b9aade3798331942b4      1
    200 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    201 0x924baf1e2bbc58e9cfe333d4c7074886a6af1afe      1
    202 0x938e202f15af8c8a782588262c3ed0565a7f9411      1
    203 0x94345607093b249b5100e8d1e499928dc163dfdc      1
    204 0x958914bc3fc61629dcc5c11ce9d2e1dc254f3e57      1
    205 0x9613a1f63bb8c3cfe6eb36cfd7b731e5fd07d322      1
    206 0x96e3baa591c9cf4d5f44dd44d02a3d4ce9290ac2      1
    207 0x9969db4034a136650cdb07955cdf5635499a4012      1
    208 0x998ffe47bf0b2b8b8d4bcbafa8fa8aed3951a1f3      1
    209 0x99980dd0184773ea492c23033e1309609a7e75fe      1
    210 0x9a659894e5d115846767db0e1685744c452e7a6e      1
    211 0x9abcd812bffc1ef2e81a2dde75b5ed8809289d9d      1
    212 0x9acb83a514399c9f5201f55ddab61481d0430943      1
    213 0x9e027bc1bae517189285a207c69615d805670f7a      1
    214 0x9ede39c1dc05ae3f37b622a71fd10d7b95d8809e      1
    215 0x9f64e6a575f50d7c8c5aea4be962f86ee2c5ca31      1
    216 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    217 0xa1486dae9f01e7da3bd8efbd12af0e1be1c73b60      1
    218 0xa17bfb3a816996ac3a1e26dddcc7850663548c16      1
    219 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
    220 0xa309e257db5c325e4b83510fcc950449447e6bda      1
    221 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    222 0xa7671ac14cbc9aa6315a35235b80afe274eb3c3b      1
    223 0xa8c4e3ce1743d0f2a6c227548c982a7c40569940      1
    224 0xa93450b6e8300071e72a5da104c7bd5ed4e102c5      1
    225 0xaa43b3ee536455939ac6155993351a9b34f72ced      1
    226 0xabf107de3e01c7c257e64e0a18d60a733aad395d      1
    227 0xabfb3a0a0e4c1bc2ad036f4531217e2becd215ee      1
    228 0xae220d647426e368ac321efec83a9d696bf84e7a      1
    229 0xaed072bbd1e541102ad6173352cd72dbb71147d4      1
    230 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    231 0xaf8c805a51ae53c3247bd6fe571a1c9aca3a2584      1
    232 0xb01b3a335db49192a31af43a88c458e5070ca5e1      1
    233 0xb066a5b94c4d1c7c06610d1628375e5e4b265de5      1
    234 0xb29548cf5ab742069387c1f4599cdca5e0273e79      1
    235 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    236 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
    237 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    238 0xb774f65d9eab67c9a587eadb5b79d0a02bfa5c42      1
    239 0xbb5cad97dc15e8c3fbe457fa65cfb38b2dfd1002      1
    240 0xbd6006b93f10b484d22d5a9d4e17bfd0ade4f614      1
    241 0xbdc29767d8bc6dd6b08d64d74c8ecf11b3f5ccf4      1
    242 0xc35152b4df68fbda0f5ba3603c7ebc113ac2ba76      1
    243 0xc449f005667bef849261b35accf931a4bace48fb      1
    244 0xc97cc6cc4b07ad3f5919165c99ce43437d6114a1      1
    245 0xcab81f14a3fc98034a05bab30f8d0e53e978c833      1
    246 0xcb7a1956fb6617bff6ee5b8b470a80b23cabe37f      1
    247 0xce56a746edaf84e65aa6a2bbb45b2328f6a99935      1
    248 0xcfc9be33af04bb18f6e7bec4e1b1ffcd06900842      1
    249 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    250 0xd00911c7f9e5993ea0cd28cb615c6b21a0101666      1
    251 0xd170ffecc42ed18ce1a163c5ce6087a4c2a68bee      1
    252 0xd38e1fb60cd50cf9ae747ecf646b4ff1a310ba55      1
    253 0xd530282d853169a23822762af0dcab045d6295d3      1
    254 0xd5a498bbc6d21e4e1cdbb8fec58e3ecd7124fb43      1
    255 0xd5ff53f48f14e9409b581e41a4cddfe0e97dc724      1
    256 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    257 0xd6f6dcfb441fd148848d76e22a4db9c84ba2d8a0      1
    258 0xd70150a7a5a42d4cd25974dae077a79a1547fcf2      1
    259 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    260 0xd8da603456bea3f46bf22cea67a510e25d3da918      1
    261 0xd9c09e1ddacce4ee6548b66773bf4e6fe9dd0882      1
    262 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
    263 0xdc36237208adb46959e77a0052843ce5446afab4      1
    264 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
    265 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    266 0xe10f736985a42ddf342f75c83e0fa457077b2284      1
    267 0xe1e57c6d779adea0f34cd3b84fb6bae3a54d1cfd      1
    268 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    269 0xe41fef6d3b5f174ae306b17e80e023915d9ac093      1
    270 0xe44946a036d9c1f8438d4d2a33acd969d8c48706      1
    271 0xe5110e73cc510cb6aec9a71eef74d27bf4f9bb38      1
    272 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    273 0xe6c26d2c2376e8997057fdc9ec0076249a6851ce      1
    274 0xe7079eec020ddfc3f1c0abe1d946c55e6ed30eb3      1
    275 0xe8fa17f9956e859cfd013b4c7df33cd704f0e7fe      1
    276 0xe95bc7ce8b9dad12eb427c8d486f47800b0b0024      1
    277 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    278 0xee620a0991d57f464aad452789a4564ba51245e8      1
    279 0xefcc4c68e1ddfaa4f0fa3a7479f0fb082f96a56b      1
    280 0xf00f1adc558ebe212d80f98e056145ca583950f0      1
    281 0xf01bcb0090cd0f734688ce77ae067da58e9d8005      1
    282 0xf0753b5515c095cdfce5a0d58af15dc5aa46fa94      1
    283 0xf0cf0a85c08527d8207a3ef45c5dc5af38a61da5      1
    284 0xf1133ca2ff1b4e5df7121566c0520199f8937e78      1
    285 0xf2439241881964006369c0e2377d45f3740f48a0      1
    286 0xf253c885f7e5f2818c03547b16f5e9068d4735aa      1
    287 0xf2bef831670df52ae5492dcaf6ae62aac86f6cc7      1
    288 0xf3bc1fb50a0f7a4b0805e4de5d5ecb8523f756c4      1
    289 0xf3e6fbbd35e2ea84bdfdce9be96ffdb2b8bd1fc8      1
    290 0xf4fd230e3a0c4bb518185880290bb6131e019e31      1
    291 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    292 0xf5851672ab7fc8729de7472dd273066b5e3b4de5      1
    293 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    294 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    295 0xf981b1da85d5db8210cff484e364881e2d5cfb5a      1
    296 0xfbb494b311f8790072d6f2d5ed1ab8695ea8890e      1
    297 0xfe53dccb4db09e660b2dc5ec48eaff8bc18124c8      1
    298 0xfe73f0b55ee4b411d7d1e4d5d5d4f8834064e2b5      1
    299 0xff41da21ff3c36ed08b2640d499b6943b881ca35      1
    300 0xff9ee049afa0389e6358a6a20548dcb0cf7a12a9      1

## Allow Gradient Phase 2

``` r
c(allow_gradient_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 59 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x06c96dc43b739cd1d7428d4cf29c3caeb14d7e82      1
     2 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    16 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    17 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    18 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    19 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    20 0x59068075a799594db03c0255eed68e8e121155c8      1
    21 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    22 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    23 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    24 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    25 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    26 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    27 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    28 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    29 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    30 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    31 0x82139687faae8a29851902783e02e699de0e0846      1
    32 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    33 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    34 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    35 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    36 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    37 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    38 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    39 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    40 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    41 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    42 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    43 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    44 0xbf814810b44978de273191fd612aa47f7b69d564      1
    45 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    46 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    47 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    48 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    49 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    50 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    51 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    52 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    53 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    54 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    55 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    56 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    57 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    58 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    59 0xfd22004806a6846ea67ad883356be810f0428793      1

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

## Allow Memes Phase 2

``` r
c(allow_memes_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01e8e9927d7c6b71671865f05783c8cbe04cc559      1
      2 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      3 0x080ffeaf914180e18f69092d66de11925434b540      1
      4 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
      5 0x0b9f898921b2bb8cd2d7d30faffec2c90200cc8c      1
      6 0x0ce6a432eaf0fa856a7c774170999db207748240      1
      7 0x0db7b27b7a3ea8035d3f66f3ba7532221ef06abe      1
      8 0x0e2f031a1142ab3919685cf82aa764d9c5c0ea86      1
      9 0x0e757c27de6feed6d9f43120943ef07d89335483      1
     10 0x0f78afe5b1e689cc2b205a78531957a286c42511      1
     11 0x0fb4e7dff29c22029975373d1bbf5873ffc3c61a      1
     12 0x118d6f5d61a9d881d452510f29c703abe9d80cdc      1
     13 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     14 0x12a0fc764430a24833fde0310fce8071e1b5da08      1
     15 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     16 0x14bee91efb79a8eb2332c31177fd3a61481ccc99      1
     17 0x16dee223fc168abff7b979813cdf15866eed7e8d      1
     18 0x18eb4aece6cff56f3bd4ccd844491d55b6ab21d2      1
     19 0x1cb89e486db5774ba084f683796286848df489d0      1
     20 0x1e6ef3e23dbf38428e752a7293b698b189c7317f      1
     21 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
     22 0x20aa168e6c793646f60737399c8466dd643d4281      1
     23 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
     24 0x2595aaffb353e812bb5cbb63f7f62e36fe528f02      1
     25 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
     26 0x2c23b2ea134c3dd6d9a48676a9a41c6ade71adfc      1
     27 0x2c3c45a64310519849c3304c8fcac3dbd14a758f      1
     28 0x2cac9e6d1d630a35cb8019779daf4c5fd96135ca      1
     29 0x2da903666829f302b0501f76144339213259c260      1
     30 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     31 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
     32 0x30f2a414945ba487f6a9ca909d0cc0919c6a1812      1
     33 0x331b4a1242ad6a794ef127bd61fe57d7bdfdbb80      1
     34 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     35 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
     36 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     37 0x3bd835333aad77686595d734ad5b80934d8b026e      1
     38 0x410ff1f298d37046a1f7e0c07fce1d8e9f91d15d      1
     39 0x42e5abe19ba467b0b32a4d042d3dfd4ba2e196af      1
     40 0x455ce1afc1403b728789b4a8c5aa512600b668d8      1
     41 0x4581c619ae0556b774f71adab6831a86da1aef17      1
     42 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
     43 0x4808d5a2b6423abc0060465fdfd338c58e758690      1
     44 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     45 0x488db5b2f8a9fbb9f50ac113ce9f88c721fd4eec      1
     46 0x48be4681972473b498e8b686d38e04826c26fc4f      1
     47 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
     48 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
     49 0x4bfb94bccfd860e1f9d85c10a8949a722676fc4a      1
     50 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
     51 0x4c8a8c3fcf77f37101d25930e7a086b4e0ec45ce      1
     52 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     53 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     54 0x4e8849962c43d9d7540f64b24bf76704926926ea      1
     55 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
     56 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
     57 0x50f8c08b0124092e1001b355f4b8ae2df85f715c      1
     58 0x513e0a472522e4bd8023c032bbf01e7fb1fe0df3      1
     59 0x526ef837f56eb025908ba91811a6ed0a3b32c560      1
     60 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
     61 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     62 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
     63 0x5d5fe1910a289b24455af49ee3e41405f94b921b      1
     64 0x5d89737e854c860d25e106c498c6dca0b516ed7a      1
     65 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
     66 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
     67 0x60bc11840966ae3946ad1904a8efb6622226be25      1
     68 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
     69 0x62c868061cddbe38e01e7a8295fdcec72c10ca06      1
     70 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
     71 0x632734882ed0127fbdf2666478df42aa916bdc84      1
     72 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
     73 0x65472e9ddd190e5b8d8c92efdf59a2d329fc9e81      1
     74 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
     75 0x660105ea6435e8d4c887a3c830b8812746eada30      1
     76 0x66567f4ec7743e58713f44f083da3de78a52556a      1
     77 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
     78 0x69cc363055d8c3d5e211865e805e145ab9208d57      1
     79 0x6df000635d86613626c5208d7c9d71b84e091220      1
     80 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
     81 0x6edd8fa5550e29d47641f969d2ead3decaa29ba2      1
     82 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
     83 0x72f52de160ece454a2d75a410f85f4978045882d      1
     84 0x765718a8e4a5562be34b122af259ce5a71372e12      1
     85 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
     86 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
     87 0x7b86a910a6c797316dda30b78098e331e08b9f34      1
     88 0x7b8a6e7777793614f27799199e32e80f03d18dbe      1
     89 0x7c76cf8894d42ed11b53236da54f05add1d44073      1
     90 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
     91 0x7f9bea812b9b6c3c4b74ec8aae849a5745cc3ffa      1
     92 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
     93 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
     94 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
     95 0x892eaf46a9c1cb75c3f230420dcbee6320c26932      1
     96 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
     97 0x8b6a3e1f151fbd8a45539e0942918e63d35c6cf4      1
     98 0x8f4b933491e1319799229ed9d36b179bb859d705      1
     99 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    100 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    101 0x92c75284b0d863165a8b33e212f02ffeecb2853e      1
    102 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    103 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
    104 0x9617562c6f338ed18e2e8e88cbbffce6003bae0e      1
    105 0x986d1bfc94671b5633d90d0540820bd3813e3a50      1
    106 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
    107 0x99f8f74b1808bd7f1e9e76b7d82151b35dfdd6ee      1
    108 0x9bc66bb0f94a38e55809dcf7eadfd6bf98d6e184      1
    109 0x9d52fd3f58557f3b5fcd2b10314bf566cabca60a      1
    110 0x9f49288c4658660c82dd98019f600a3d35969fd0      1
    111 0x9f9ca0285dcc35cc6831652e79c029fd0ed4bc75      1
    112 0x9fc80955aee9e3eb8c511a50a72b7e589700ffd6      1
    113 0xa03d04ff89308adf54badf18b46fee9980093524      1
    114 0xa0ff0e41d6847b1fce33a721a766e9e040027e6e      1
    115 0xa10378da8fbb61b408e7816b3e85bb003c485787      1
    116 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    117 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
    118 0xa222204acf1be4077d34102fab38a759060b77c2      1
    119 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    120 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    121 0xa73769aed346319287410811639ac3bec8464d55      1
    122 0xa8450d71b98ca4955c63519ef593ba886c5b2b4f      1
    123 0xa8806ccb79069c976e37ed1950ada0eacef6a289      1
    124 0xa8e54b46ae93e14eedae486a9efcd4c7b5a5be20      1
    125 0xa8f1b7bee4bb60311511144ecd6dab7b04ef2735      1
    126 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    127 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    128 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    129 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    130 0xb29479ef88dfff8dac466cf6c716e7985c4241c5      1
    131 0xb2aadf6bfc0a5213acb9c279394b46f50aea65a3      1
    132 0xb40969f60bfa246a09593099dace2ddd543254a3      1
    133 0xb53ee69b92ad6d12e6f3b1f849ad3e706e31a263      1
    134 0xb59a6d337f8d687447fb311b8138340b8c617715      1
    135 0xb70c3049982c09c18dcbf8596ccef6f5b3c239a3      1
    136 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    137 0xb8dfd425f4d6227036d5342cc4ac2b90826e1b05      1
    138 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    139 0xb97624935dd3fb84e5492e8d01c6fcdce8060cbc      1
    140 0xbb59498b19783e5d81f72ad07acdac619b6808e2      1
    141 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    142 0xbe67847fed0f9760c36c6c627c513375673781f9      1
    143 0xc02f533a819c4d3149544dd1a55cf7cc87a8d30b      1
    144 0xc03e57b6ace9dd62c84a095e11e494e3c8fd4d42      1
    145 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    146 0xc13d5024c2ee14c5f80847afd09275f8b550a135      1
    147 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    148 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    149 0xc31ba2d3275a397c43b1a4ab707cd79adf310233      1
    150 0xc3c1744bccfe4691e332c873f9cb99b53214e03c      1
    151 0xc45920062985116eaac6589058ed337066d6f2e6      1
    152 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
    153 0xc7e8a6abb9b5c76c75c9bb4f77715793f7f8205e      1
    154 0xca288359ba6319c7e7c7377a670183acb3781cda      1
    155 0xca3dff8c740dee29528916eb049cea48f500d387      1
    156 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    157 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    158 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    159 0xd02cf646520827d74d4ff0435f96220819772b4b      1
    160 0xd1386560f0fa070b6b79e9968e8197cf17f3b8ae      1
    161 0xd1e508c60d923cc9e93785da0bffe01710ccbac1      1
    162 0xd34e3c34ae9828dffedb9a2f236af47119a113bd      1
    163 0xd7192081e5f481364c190022f0012a729fba37a5      1
    164 0xd72c8dd33954e8aa53d5c108906b751ce4b2382b      1
    165 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    166 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    167 0xd9e2ad004ac82915d7472448cef2b182547487bd      1
    168 0xda39876a118f1690e584351dd01b785a8f1297ed      1
    169 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    170 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    171 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    172 0xdcab53e3c8a2c37cc5e2509c2db0caa6c04f3ce0      1
    173 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    174 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    175 0xe061ea94f07de987a55a49067b0d7ca3feaffbc7      1
    176 0xe07caa87dfc96fbc74f4c113270cd385574d5bde      1
    177 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    178 0xe23e0bd6e320c87367b0e4b797b42dc9b4fe7ca0      1
    179 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
    180 0xe29bfd5761a18b5c3ac4a4271eb7fadd4c7fb942      1
    181 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    182 0xe3013b1d58b257684ed9862dbeef81d863e2e0f3      1
    183 0xe3d9a03a6422b56e7097ff41a86045683f0aa150      1
    184 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    185 0xe8eb234375d59df64823ffda70207e26334ceeb5      1
    186 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    187 0xeb775bf133c142e873f2ba6925d53107550e8703      1
    188 0xec034c3abd17fbe51b10283d0902a1210041fc77      1
    189 0xed6e663b8d2192c515ff70ee0d6291e44db83be9      1
    190 0xee58519040de3504932f4c79e1d056ef1b42a026      1
    191 0xf1a17ba0d48798a3cc2bab1fb3cac942c4d6817b      1
    192 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
    193 0xf6ec5b0d097178a0acf493afdcda2a54545ab0f3      1
    194 0xf6f85d9b96a43c87fd29e2facbf644df6bb029b0      1
    195 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    196 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    197 0xfd17019d6a7ddc7ad585afa68dbef71084162601      1
    198 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    199 0xfe8312a959e031c7d4cbe3f9cdd3ec8726d0d80e      1
    200 0xff3bc8de74bb1d2f9066c9687f62bf810c66c5ea      1

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
