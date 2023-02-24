
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16689869.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:19301       Length:19301       Min.   :1   Length:19301      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :3                     
         name          
     Length:19301      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16690069 # https://etherscan.io/block/16690069
block_hash <- "0x9034b445f36046b1a58677703fb88ddc8f9ea17b62e3011d62bc41a0d10793fd"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4500 

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
  "0x000000000000000000000000000000000000dead",
  "0x0000000000000000000000000000000000000000"  
)

hodlers_remove <- c(
  ""
)


airdrop_memes       <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=42,address_max=1)
airdrop_hackatao    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","QueensKings","QueensKingsLAB","MakersPlace","MakersPlace2","HackataoOnChain","PixelChain","PixelChain2","OpenSea","Hackatao","Asyncart","KnownOrigin","KnownOrigin2","KnownOrigin3","Voxels","SuperRareEditions","MakersPlace3Editions","HackataoOnChainEditions","OpenSeaEditions","HackataoEditions","KnownOriginEditions","KnownOriginEditions2","KnownOriginEditions3","heroines","NiftyGateway"), address_remove=address_remove,address_pick=42,address_max=1)


allow_memes       <- pick(snapshot, contracts=c("memes"),address_remove=address_remove,address_subtract=airdrop_memes,address_max=1)
allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_hackatao    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","QueensKings","QueensKingsLAB","MakersPlace","MakersPlace2","HackataoOnChain","PixelChain","PixelChain2","OpenSea","Hackatao","Asyncart","KnownOrigin","KnownOrigin2","KnownOrigin3","Voxels","SuperRareEditions","MakersPlace3Editions","HackataoOnChainEditions","OpenSeaEditions","HackataoEditions","KnownOriginEditions","KnownOriginEditions2","KnownOriginEditions3","heroines","NiftyGateway"), address_remove=address_remove, address_subtract=airdrop_hackatao,address_pick=250,address_max=1)


allow_hackatao_2    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","QueensKings","QueensKingsLAB","MakersPlace","MakersPlace2","HackataoOnChain","PixelChain","PixelChain2","OpenSea","Hackatao","Asyncart","KnownOrigin","KnownOrigin2","KnownOrigin3","Voxels","SuperRareEditions","MakersPlace3Editions","HackataoOnChainEditions","OpenSeaEditions","HackataoEditions","KnownOriginEditions","KnownOriginEditions2","KnownOriginEditions3","heroines"), address_remove=address_remove, address_subtract=allow_hackatao,address_pick=200,address_max=1)
```

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 42 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     2 0x111818a51c4177e8980566beea68fe334be7b76a      1
     3 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     4 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     5 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
     6 0x220da3fc3b905c968dfb20c81c170bc4dce56603      1
     7 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
     8 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
     9 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
    10 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
    11 0x3bf55d8aad7ca0c5cfe1f697d7783437b9e034fb      1
    12 0x431181dae361813567f35ee2abac73291820fcc9      1
    13 0x488e5685b38d9412cdadae46feed3e863f57ca5b      1
    14 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    15 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
    16 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    17 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    18 0x5d25087405105bab12624c73488ec186066a6376      1
    19 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    20 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
    21 0x7729cfd6841ec449c20fa7d3970d2e0d035c856c      1
    22 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
    23 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
    24 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
    25 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
    26 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    27 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    28 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    29 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    30 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    31 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    32 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    33 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    34 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    35 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    36 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    37 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    38 0xd4e791d799b8873d86ac06a5fedf2f1a75f639be      1
    39 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    40 0xed690606f908361c26bd2b1ad4fa31ed201dca4c      1
    41 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    42 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Airdrop Artist

``` r
c(airdrop_hackatao) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 42 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0439ebe6168c80ff8caf438dc902090b77326555      1
     2 0x1246191c1bbcde42c1ee83e3745ba8ab37478827      1
     3 0x13abb285529729ed8acecff3da52351e991f650e      1
     4 0x1527fa3a0778d574adfe714b7e60c7b2a1bf7c29      1
     5 0x159ebf9863e44f9d910c0e8d8948cc91b803c168      1
     6 0x2a26fb1a180a2ce5c6c2d06dc4d430c37e7ecf39      1
     7 0x36925330dda72070aa7670d1f1505bf77e25643d      1
     8 0x37a464ee47e6706b4a5a23f74ce9673615e9ad40      1
     9 0x3927af597c09774f91ef84bf765ded4109e9f8ea      1
    10 0x42377a55f958e2125701fd92df42aa0d274ca6c1      1
    11 0x47a3443937af19aca502042617165767b278ee35      1
    12 0x4986bfb143e2afc58284e33253289b9dbb270957      1
    13 0x577ff1df8c33f95c6180bcd7b56251a9d1f3422c      1
    14 0x5998d1e62b43498b72e89613c1c33944492091cf      1
    15 0x6089ef93f1e24d9bb74cb6989d457e67f3d317b3      1
    16 0x6a4701c6df709f0b4ff349b8be36d2424cd116c6      1
    17 0x734ed31ad4853ecb36e8554ec35408652642f3a0      1
    18 0x783d71028044c5e8a6c997b705284865d02f751f      1
    19 0x821d86d48c623511b2943f7112ab62e8adb25228      1
    20 0x85c0c09570c955f6e07522e9ad6041716cbbb7fe      1
    21 0x8753982d800bbbf1faf478ef84c3e96f2775d3b9      1
    22 0x8ad272ac86c6c88683d9a60eb8ed57e6c304bb0c      1
    23 0x8af30b3ff1c29119ed336d45f77a6d59c3273b92      1
    24 0x8af60da4e29007eb55b366e89b6c8ec7a64ecc8d      1
    25 0x8f3d381a2df65b5b35496be5ca9e48ba4ffad34c      1
    26 0x8f3ddf567bff663d61a12a5d5f42f2e6f220f89c      1
    27 0x8f5c1e85e70590cf8500b878da27cd1051b783a7      1
    28 0xa3b39a8d46bcef5c823584680883b79690de933d      1
    29 0xac79eabcdda00261a45c35d71e4466ba375b6acf      1
    30 0xade6cb680030f97cf612ff7694b1e4c30ba27735      1
    31 0xaef48e7fb14048cc2438745165b6f72157d89cac      1
    32 0xb811dc71af7d9242a161d43318a5ca685f56f680      1
    33 0xce28c87f3c9dc0dafedb3471318bca748f1e4479      1
    34 0xd1f6c71350791f2c26de8c0e2e5f293a83455c52      1
    35 0xd288767f458ea4456f6f20e2b08303932f9f4475      1
    36 0xd42aed8d89715d2576d3d2bdd81d6d8bcf6ff270      1
    37 0xd58518b059895d0628f065f42d2d90d37f347f1a      1
    38 0xdc64332073c58ad4e2ff71c87883286024b086af      1
    39 0xe538bae1a0fe4048e92544b3c67e5e0f340830d2      1
    40 0xeb6599cc9681724137c24bd04add50fad0bab11e      1
    41 0xeea254f9d7f096ccab5911927a0550bd1e332909      1
    42 0xfd9c5f1377692c158c56c150cb4c84ea1226f56e      1

## Allow Memes

``` r
c(allow_memes) %>%
tally() %T>%
readr::write_csv(file="allow_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 358 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
      2 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
      3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      4 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      5 0x04294157cfba9ff0892f48f8345ea3539995f449      1
      6 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
      7 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
      8 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
      9 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     10 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
     11 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     12 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     13 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     14 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     15 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     16 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
     17 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     18 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     19 0x0dd38657d7c8024e7d62bde3d0423ca34769be50      1
     20 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
     21 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     22 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     23 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     24 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     25 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     26 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     27 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     28 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     29 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     30 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     31 0x167a4e6066d6c96d5c012006f5cffc9f606131ec      1
     32 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     33 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     34 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     35 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     36 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     37 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     38 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     39 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
     40 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     41 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     42 0x1c172d05b75178fc669d74407243cc932030f139      1
     43 0x1ce68e5912ef8cb4e83620a50b70edc900ad4759      1
     44 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     45 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     46 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     47 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     48 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     49 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
     50 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     51 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     52 0x23602ca06e977c86339ffddad74966e824ab691e      1
     53 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     54 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     55 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     56 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     57 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
     58 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     59 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
     60 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     61 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     62 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     63 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     64 0x2d8b81cde837334beb89a9c0a88179ad234770be      1
     65 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     66 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     67 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
     68 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
     69 0x30eedd4ea7e1959fc9d6879cd26f1d8baabbc5f6      1
     70 0x313c8673ff69dfd7be2a46bc540305d3e2d233b0      1
     71 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     72 0x334cb32bc18ec632ab04ee55157aae1ff165e093      1
     73 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     74 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
     75 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
     76 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
     77 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
     78 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
     79 0x3876be5be4998adecbfbbad26604a762467e7f42      1
     80 0x388160a99390392278afdba240046b8b5e73f77b      1
     81 0x39bba3c54a3412127bdfb0f218efea52b5d28300      1
     82 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     83 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     84 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     85 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
     86 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     87 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
     88 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
     89 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     90 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     91 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
     92 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
     93 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
     94 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     95 0x417c269b82387ab605bdfbd312d91baf03dc8516      1
     96 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     97 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
     98 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
     99 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    100 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    101 0x44f301b1de6c3fec0f8a8aea53311f5cca499904      1
    102 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
    103 0x47a79ee280b4a6a8d68a3ba16e4082a765a2b20f      1
    104 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
    105 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
    106 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    107 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
    108 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
    109 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    110 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    111 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
    112 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
    113 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
    114 0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3      1
    115 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
    116 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    117 0x527bb834cc5c8ff730c673880e51372282b06e14      1
    118 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
    119 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
    120 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
    121 0x54913cc8ea17731d62589039dd0152f306473843      1
    122 0x557c60995797fa7b47be105227a2e46148d85750      1
    123 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    124 0x58059014c5952c04370bcb88a2e0503e9eafb209      1
    125 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
    126 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
    127 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    128 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    129 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
    130 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
    131 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
    132 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    133 0x614b89f072ea263a9387460963142e73548fbaf1      1
    134 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
    135 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    136 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
    137 0x64f02c24bb510e27b5eaec705eaa840ff7b4df15      1
    138 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    139 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    140 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    141 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    142 0x69cd43dd4ecf3a076b1b9d2cfd41589987129bc0      1
    143 0x69e68074f1aada957edd39c5eae0069973343f30      1
    144 0x6b29380bb9955d5b2df1624c5c6a72a5e69a6b80      1
    145 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
    146 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    147 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
    148 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    149 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    150 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    151 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
    152 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    153 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
    154 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
    155 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    156 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    157 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    158 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
    159 0x74d50ea9f8cf5652e75102c3fa049c01534dd6c4      1
    160 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    161 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    162 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    163 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    164 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    165 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    166 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    167 0x7c84aab9f3eb0897191d95afb2aeb56a63973532      1
    168 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
    169 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    170 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    171 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
    172 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    173 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
    174 0x808421753a181e96812796b7ab43d3f356cc5a77      1
    175 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    176 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    177 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
    178 0x82139687faae8a29851902783e02e699de0e0846      1
    179 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    180 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    181 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
    182 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    183 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
    184 0x8874174a2366668d54fea6343f71709389563c8a      1
    185 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
    186 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    187 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    188 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    189 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    190 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
    191 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    192 0x8d12c02671848b17c18322027a2578ea7afbb702      1
    193 0x8d5e59e11838cff72af5fb0681d96a9136ad0604      1
    194 0x8f160eb907a80517a3fa6d22f7cf20f552696a44      1
    195 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
    196 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    197 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    198 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    199 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    200 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    201 0x94f052ca65a34d7f581bba95a749b1cf52689dd5      1
    202 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
    203 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    204 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    205 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    206 0x98004498c85c4b1997b3f669475a038bbcec2160      1
    207 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
    208 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    209 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    210 0x9caed8a01c87721570de895a9725334a0a0cdb70      1
    211 0x9e1e3857fb2484379858b9daf230379015a7a100      1
    212 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    213 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
    214 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    215 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    216 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
    217 0x9feba45425c51b03890604d6ad51f4ee7c9d4866      1
    218 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    219 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    220 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
    221 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    222 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    223 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    224 0xa59422ec0634a0263bcdfce5e29dd2df3803447e      1
    225 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    226 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    227 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    228 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
    229 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    230 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    231 0xa97204eb6b0f3e58e8acff48a2317511e7628949      1
    232 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    233 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    234 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    235 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    236 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    237 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    238 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    239 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    240 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    241 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    242 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    243 0xb1c11a879c47debc22e3816e7b727fc8bbe3c8ac      1
    244 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    245 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    246 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    247 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
    248 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    249 0xb4627672ee52660a9e453ec541834e04583f3602      1
    250 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    251 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    252 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    253 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    254 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    255 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    256 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
    257 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    258 0xb9ad9d091c6841e640dba4cab02baefaf1134cfd      1
    259 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    260 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    261 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    262 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    263 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    264 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
    265 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    266 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    267 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    268 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    269 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    270 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    271 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    272 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    273 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    274 0xc522289168311a765cf17c067f0118578c99cf08      1
    275 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    276 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    277 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    278 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    279 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    280 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    281 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    282 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    283 0xc94a284808e31b9ef43f9c9a48933e5a268cff9e      1
    284 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    285 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    286 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    287 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    288 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    289 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    290 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    291 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    292 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    293 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    294 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
    295 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    296 0xd0b2d2d86a45bf2f13a3af83df6e7b5fe98050f9      1
    297 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    298 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    299 0xd1dd467f1630be10d67205d6c816429f8ee49124      1
    300 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
    301 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
    302 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    303 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    304 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    305 0xd5ec003289265705727b622f1700fe814e54ca67      1
    306 0xd6b2735e290ba3726d25c9762098cd26a16f023f      1
    307 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    308 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    309 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    310 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    311 0xdded73b0bbdb3782a453c9202f54c746fd391068      1
    312 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    313 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    314 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    315 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    316 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    317 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    318 0xe25b24cebed3055236e369570a437a99e1d32602      1
    319 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    320 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
    321 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    322 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    323 0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1      1
    324 0xe7f31a4516dbebc0f443a8d4ac8b22d1a8f096e0      1
    325 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    326 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    327 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    328 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    329 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    330 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    331 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    332 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    333 0xeeb9148a32b45672e16591ee18ceabc773b099b5      1
    334 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
    335 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    336 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    337 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1
    338 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    339 0xf054274dd74987395d28136e53f39ef4f7b19994      1
    340 0xf12e159643edeeba920518cc614820ab5726335e      1
    341 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    342 0xf1f476f144df01480297dca47efca565e8b0c9f1      1
    343 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    344 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    345 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    346 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    347 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
    348 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    349 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    350 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    351 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    352 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    353 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1
    354 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    355 0xfc5ef50b9d7a080cd620f404efdfa287af9a3ac3      1
    356 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    357 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1
    358 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x06c96dc43b739cd1d7428d4cf29c3caeb14d7e82      1
     3 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     4 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     5 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     6 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     7 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     8 0x1566ae673ae80725bcce901b486c336e6acef465      1
     9 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
    10 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    13 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    14 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    15 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    16 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    17 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    18 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    19 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    20 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    21 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    25 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    29 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    30 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    31 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    32 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    33 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    34 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    35 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    36 0x69e68074f1aada957edd39c5eae0069973343f30      1
    37 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    38 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    39 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    40 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    41 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    42 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    43 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    44 0x82139687faae8a29851902783e02e699de0e0846      1
    45 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    46 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    47 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    48 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    49 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    50 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    51 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    52 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    53 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    54 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    55 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    56 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    57 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    58 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    59 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    60 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    61 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    62 0xbf814810b44978de273191fd612aa47f7b69d564      1
    63 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    64 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    65 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    66 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    67 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    68 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    69 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    70 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    71 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    72 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    73 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    74 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    75 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    76 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    77 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    78 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    79 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow Artist Phase 1

``` r
c(allow_hackatao) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 250 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01b41056fa643f2391d34fb5a1d4876cc3e692e4      1
      2 0x04fd85428256e7b2e35f68432470cde04ef7525b      1
      3 0x067432789aaa5072164fa8af69361539b7fb112d      1
      4 0x068950058669485519fbdbe88b6d0f83bb4884cf      1
      5 0x0a65b47e0544309f896d46e3e07e26fdc169c79e      1
      6 0x0b8be030a1d151b1fa722a794eeab43a45b1e002      1
      7 0x0bc8cfab9005c1835696a4a0800a2e037ba47ade      1
      8 0x0cde89a91d6fd05c35f593f029b2be2d5b5eb075      1
      9 0x0dc903afb13b941e2967f4cb1c37bbc91f5ccaa7      1
     10 0x0e2f2c0698f18f5620a2a279169830e4c42d9c14      1
     11 0x0f18c529b51851079b348bcffd1a45c9426fa65d      1
     12 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     13 0x13c75cfbc769b53139593f8b0966539a92204d58      1
     14 0x14202c79dc15cebba5115adce8a457f49bc71120      1
     15 0x151912e86d5afa1be0c8968ba4ad1ec7937da975      1
     16 0x15a415c7589efaf71345006d33bfa07a9a42c8c8      1
     17 0x15e03c94d02d6be593e82fed43872970a8a07c1a      1
     18 0x162a64e9988479a960d943418479152c58e0c884      1
     19 0x1689b564814db5f1c7abd74129771d282e2d652b      1
     20 0x16caab5371ae1ff9fb01617769ddeeb52460951e      1
     21 0x17357f340ff06dfa3976b41b182ae03d579767b4      1
     22 0x17f197bf61aae29472e4f14d9e039ffc926e56ae      1
     23 0x18c6a47aca1c6a237e53ed2fc3a8fb392c97169b      1
     24 0x18d89aaf01f71c2c3ccb03861d2c9c37c1ad7d9a      1
     25 0x197db019c268886973c81e1f6063bf4feb539d82      1
     26 0x19847a32b0eb348b006c79c1fb2d3ae1276c6028      1
     27 0x1a7861a39e74d93c8eb883d774f168be46f81228      1
     28 0x1ac74c6f823782f0c64b94bb653b0292d211309b      1
     29 0x1c26023f36d9e9f21a09bb5b7cc260dfbae98046      1
     30 0x1c8915f70850873a03df2d6cdba71b226bcdfeb3      1
     31 0x1cc1983396e55e925b777a0d70dfe8b56edee942      1
     32 0x1d6bd644bf98b718e260ae53ea224465c7a72b40      1
     33 0x1dc3a1f1a432d7766bf67bc20a1c4e56bf727ad2      1
     34 0x208b82b04449cd51803fae4b1561450ba13d9510      1
     35 0x21301d901db04724597d1b6012ac49878157580d      1
     36 0x228f9fd74f35080771a9e858f9575451d3ccf7ed      1
     37 0x235f32bb3920bf99e9fa813a37fcebadaf9cdf39      1
     38 0x2378d2cb86db464bdaa7f481ade37b191df2a3ff      1
     39 0x24247c0a091b7e8ccea21f913cbb52deb7311e88      1
     40 0x2545d11d013366d85d38ed6d6bbf1f86d2aadcee      1
     41 0x26613696bd07c7a6e43c94ea457584f1a5d9f979      1
     42 0x268f5fa2adeb3a904fa60d4ffb904738f0dfe3b4      1
     43 0x26dbcf7bb0e7826e087c691228b5f337143fbe16      1
     44 0x26e2e8a24e867e5e96ea0c4913e659cdc8a97973      1
     45 0x28b47a4fffaf0ee72f2ce05b4b6bed6b6aab234b      1
     46 0x290fbe4d4745f6b5267c209c92c8d81cebb5e9f0      1
     47 0x299eae71c3ad6016caecbcf19530cd8828b6fe63      1
     48 0x2acc968b9e4b8ccb0636625f4b96640448f812d1      1
     49 0x2ae2feb0c531f79dc3857e1bafdb65835221e601      1
     50 0x2bbff01661b994de84134b4de7a6f3c5b7da1dd2      1
     51 0x2d888127a358968aaf86b474642d9309841f7b77      1
     52 0x2ddfa151cb51b6276982943c85ac123cdcdfa0a5      1
     53 0x2e09e088a31481d17c0ee28ee8b40b54554a31a7      1
     54 0x3041e4002267efe34a3fd8fec78f20ea4967693e      1
     55 0x309723d02a344db96c3d92b7e6ed974335c1c7b1      1
     56 0x324bb6aa62ae349d1f45ed77476d89c20b768cc7      1
     57 0x35f5a3f01bb9cac58796b2c44b310fcf97a21e69      1
     58 0x38039da6bc26c101316af426cbf7f43834957c46      1
     59 0x386843276e4dbe92d0062e999d6f54335cfd1de1      1
     60 0x3a0deb0823b23b5b7c9271fc419d95d4a70ba7d8      1
     61 0x3a177de5b77de4807fbf8b8c902912918cfc1ac3      1
     62 0x3a63afc862107fc21baf40a5025db7c4062ac2da      1
     63 0x3be3e2a1301580dd21eacd4c2d999f16fb9d9b2e      1
     64 0x3d27b577b7c2388964a7e1a397f96e913b0d48e7      1
     65 0x3dfb425b38f1109ae985731aaacc44d0a7c82f61      1
     66 0x3ee4a408d933953571cea6615a55f2873ec8201d      1
     67 0x4326804e883dab030d988fc498aec20e5b6c4f64      1
     68 0x436e31e81b8e331445bbfdb506184240f20a1b0c      1
     69 0x4383e155b97f282a22060d8f8215c0e1033a9ad6      1
     70 0x47700fd69c6cd7772c39973ba7bae5616a18dae2      1
     71 0x47f39bf851926110239200298ef9c2a6d44d0305      1
     72 0x4880d3b112c4b1db5f53f4d31dc6a26e39c81981      1
     73 0x4926b4ff7b33ba5810fa530f9ceceec5bcb9c65a      1
     74 0x4b46f1e241838a910945a3fac242ddf6f5d7c041      1
     75 0x4c2258c1f4d438b626150df7af1ddff6d5917226      1
     76 0x4eb6eff3639546cbd1c40e66f2ed118f6fb637eb      1
     77 0x4fe5d0b4b663e36dac83bcecb1e25af2bdcad725      1
     78 0x50e33cfe14b13ca02d9ba855c7deeae92ff541fa      1
     79 0x51360d99966724b2603182cc367ab9621d96eed2      1
     80 0x51734e5211c3efd7011eedae5547b93244b67723      1
     81 0x521dc78990c39f7c4554cb21655d930cf3ab211e      1
     82 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
     83 0x56b9edeededebfd94af535733c126ec1c3c3e16c      1
     84 0x5b2205bdb33b9779581d0129d9c8ad1e48e9e334      1
     85 0x5b6955d57335b9b44b3f727234960eeda93039a4      1
     86 0x5b6a8b298daba526ecb7f51a517df1fc26098327      1
     87 0x5be8f739c8ea94d99b44ab0b1421889c8b99b2e1      1
     88 0x5c19b23be05acdc13152574f54b87cc3bb3787b2      1
     89 0x5c4eab549a528bf15c47a01e174500010997235c      1
     90 0x5c7a9037152aafa6a5842dbe13086e8c496c88f5      1
     91 0x5d2e7980327832a676390103537fd8fc10fbcedf      1
     92 0x5dc610bf74b4e6b299306d70bfa7e2162b1d53fc      1
     93 0x5e0862f7d51bf98719f451451ea4b80bb5c0548b      1
     94 0x5f259d8c6564d7dae2d745eb68bff9cc6c7dc439      1
     95 0x61dd5a121b61b7eccfd765a96ae36885cabd089e      1
     96 0x61fd0d043d519f5a2bd05785000f30db96809429      1
     97 0x6242ab9b1f5f0b8e3186055af07825fdd00ca273      1
     98 0x633d2239166ae6ef8d38ce1674c8bc405e2550b8      1
     99 0x65430cb840a26058c1d070df320fb9c993338e85      1
    100 0x67808baf60456f41ada9d9aa9840c88ff325d8ae      1
    101 0x6857311ed97a0f56785159634c53deef60565790      1
    102 0x6994e8297f41e4d53fd2496ba47ae347a1f512c9      1
    103 0x69b2e9135ada9fdadc1e07ca0824018c2ff0d60d      1
    104 0x6a101c169c40c2ef812b7ee5fe4cf9ea482a2c93      1
    105 0x6a8b49af5672e9de0ef6b4cafed925616020248b      1
    106 0x6bda4108175e96051ad4fc6c9fb1adedda8f85d7      1
    107 0x6c0aff3a716292f37c79bc726964941f1d8ee819      1
    108 0x6c45a80ef5319accbbbf70ee2269c6285b4f502f      1
    109 0x6ccaddc511f4354ddef5375f9feb9c66ab9cc9cf      1
    110 0x6f6b49ea6a41681d39c1b3697d8b47b0b3706e16      1
    111 0x6ff7095144c856422c09102bf0606506dae6f370      1
    112 0x7101a39c053954e3e7fd010fdf3f6ef6bdbcbde0      1
    113 0x714696c5a872611f76655bc163d0131cbac60a70      1
    114 0x7206b857cec61f5bfcef563f9d8877da2a91dd83      1
    115 0x72ce421414ebc9f758c221faf0d40189e3549194      1
    116 0x75b0955d5727148816479881b7dd4cc646af27bb      1
    117 0x778e742df441ccde5161d55109e23d1810ff8183      1
    118 0x78866420eb0f6c0603259a4d3db93d9067a42567      1
    119 0x7896490ec41282ca6f80870448e9a0eeb022746e      1
    120 0x7915e43086cd78be341df73726c0947b6334b978      1
    121 0x79b711055b9fa9b1bb49eedbd3bc8bbbaa0d5f77      1
    122 0x7a04e11f6708d9dc4398b04a15cdb3329c449ef8      1
    123 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    124 0x7b2f44724f31f3bb099998d5f326d708a157b6d3      1
    125 0x7bef8662356116cb436429f47e53322b711f4e42      1
    126 0x7d81ebcc3234125c1ebd5ceb82b1ba53c3a26887      1
    127 0x7e08cc649ea383d45f9c24fffbcc761c5abecc1f      1
    128 0x7e8fcd1046ae351af66c12b75b051fe550afe71e      1
    129 0x7eabfe610657218bb4fd5620ed699b14de305412      1
    130 0x7f20e079198d59c6588ce9e0f533c4b5bd6e2943      1
    131 0x806a3396514a901f4b535a4b7faeb01693cdfb76      1
    132 0x80ddedf3d46c2461379465d32449421809fdc1fa      1
    133 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    134 0x844fa4379f3c92f79b32f92574ed3694c2b7b49c      1
    135 0x8473d34b2d25c447d3a8a3c54668118bd2456a46      1
    136 0x855e14c818c61e00df9f6e5164ac63e2d764d39b      1
    137 0x870de46d79f98e88c962912eb3e2984d4bc4da91      1
    138 0x8778b3120ba60fc831844c3dd2c5948c3fc6bc86      1
    139 0x88889778402dcf6cf363bb9ac47f7a2d8e77a2af      1
    140 0x8902d60d814b10f615f5401eeef6ba1069e741e8      1
    141 0x8bec6b9ba708eeed4a50e6c5d3f2757fe1234289      1
    142 0x8dac3217aaad3a26a31c209b26d6fb4a0bf1deb4      1
    143 0x8ead16bedbcadf94669729c083f27c77772ab164      1
    144 0x8f72c3b6025cebc89d58b357c9066d576cc3bbb9      1
    145 0x9127c9221b22ea3789c90383284c72dcd7d9b9fb      1
    146 0x91b79d7e40da6a5bfb81312d3813f39e91ae5052      1
    147 0x927180f41fb785ca255a32e0426d76df130fbeaa      1
    148 0x938476b5fbe0dae70d65a5e40c52a734116694b2      1
    149 0x93b21df8f52b9b86d26fa2bad50d2975159d2f37      1
    150 0x94260bb4920f0e60899e38227616e6d0d4cd6549      1
    151 0x98cb6107a67da3375f16c304aeffe1fcc0b1239f      1
    152 0x98ee235244a99fde7aadafb0c0fc77845d9e055a      1
    153 0x99ce7c2a87255c26ead8f9696d3e06b7ce3a28d8      1
    154 0x9a1535056e6617fec5498f438391377af9b044b5      1
    155 0x9cf77a5bf1986745f751c19135545f47f4eccc0f      1
    156 0x9d7d086012f6c610efa54de9a6e14ca53629155f      1
    157 0x9dc97d4080742e5f010e813343d65d37ebcfa3af      1
    158 0x9f64e6a575f50d7c8c5aea4be962f86ee2c5ca31      1
    159 0xa456e09a747488bb1c33b194cf4cb180d2dfe720      1
    160 0xa65a864ca19535e4c06000768c6cfa9f6ebaeb12      1
    161 0xa7119b6910effcc360728705ff37877705e829b0      1
    162 0xa7c5bf2b18f45bd9bf2bb7e97d4b34be20bbd4e3      1
    163 0xa931a25217ea04269e9fb9b90e2ffc72e9e92839      1
    164 0xab0e22a676be07dec0d4b7953e1dd1661ea21dc1      1
    165 0xad4d9f62a2571a1a21c7d4732061136f5327504a      1
    166 0xad8d22b89e55490e72bb5b06971f47c4b329e8b2      1
    167 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    168 0xb02365dc1a28d82cc454d05a765feee31ccc2b00      1
    169 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    170 0xb09861246e4149afa18581449c0bb6d4f56bfee3      1
    171 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    172 0xb1a50f2e2510bb1bb1b3e7fb1fcf8cd170a56381      1
    173 0xb1a7993db1b943c8ceb99e5e526fe65e133f63ec      1
    174 0xb2ac226dfe6b0d2203f34afe3ebe790342637582      1
    175 0xb385a44f5447691adf74dd16dd2490d93f564842      1
    176 0xb39734396b95660707ef7f3d3a76e8566ee34377      1
    177 0xb3e33e96bef4fdcc034caa376e6737773b120289      1
    178 0xb4e88a500affc46d863c214768433ab0fe06ab56      1
    179 0xb5af8e71e1c7d702b0be8cba90337a81817f7364      1
    180 0xb6ad61b7558c718269a9129bd9ed9839d88a126a      1
    181 0xb6d9353db4a923a378d5b1da096d04f3704e421b      1
    182 0xb84580a14e42c74efa1213ab2fb9f98da33452ee      1
    183 0xb8903d33924b10b9cda7fd9505e395f1ae0b8532      1
    184 0xb92b87a226cc740b3c5934047d3481fa87474de4      1
    185 0xb9addb7594e6047baa292e98800a29d142e93fe1      1
    186 0xbc922f980d891734b55a4a13fd094280428a6c9c      1
    187 0xbcdb3624c3101463c7ef19e8cfe94183dc45b2fd      1
    188 0xbde204a765af1131a7bfc4b1d4d2c0bb45a6071b      1
    189 0xbeb6d135c54a3d0598d7b3a5e5ad0eaa8f1e593d      1
    190 0xbf17f6d3d7df38b4179f69059bcc9b1cbb8a278f      1
    191 0xbfc007c6d7deb895643bf457060519137bbd2c8c      1
    192 0xc23b78f47cbf3f4ccf3bfe5899dd6a20c308ceb5      1
    193 0xc2c32c963064c16281245b1f7e6bcce6c1089716      1
    194 0xc3d88d230778c1edd6884aaaca3499fd912ecd0d      1
    195 0xc6e89a98aec81debbdc2105ebc6d070f6b0f7f22      1
    196 0xc842ce7214a14fa98186a010bcb43c7e99e4caf3      1
    197 0xc856ce8d6cae331f230f0eb1723c53d73fe88098      1
    198 0xc88a90e06f4b47631af60d1bef0cf3bb5cd3f1fb      1
    199 0xc8e7817c4ac86ef3afc525f150091391205b727c      1
    200 0xc976052b6a1b550cb6c8c7bff80bdc5e92ba7329      1
    201 0xcb45adccd82d5379124702ba72904a43a5b6f584      1
    202 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    203 0xcd10ba4819d555a03b860a46c76b2e5b8868dd1a      1
    204 0xce4c8a44881d290865598efb51c1a05ecb8a8324      1
    205 0xd058f7f3b55cfcc600e1f3852829d3a0d07c3e5d      1
    206 0xd064c11cc1c0229fab66e81e06aee0d8ab1988a4      1
    207 0xd077a95ea7617f9d844d5a12225914f937520284      1
    208 0xd0c0650cd08acd4e9553c48c60c94be04fecce43      1
    209 0xd0f45963ab1474327d3ff36e0af962928b451a87      1
    210 0xd38bb767d1d0cb60187609a0486b2c1dd32c21fd      1
    211 0xd3c12a5e90555a4880c2c6d83f942586fd5487e2      1
    212 0xd6b8dd8abe75c848efaf77fd86a3b82d51bbac1f      1
    213 0xd7b6993f2bcab854f9bd407bda2feb28fe2c8ad3      1
    214 0xd7cd2169e4dff6aa8a611b16f659c104e20af9e6      1
    215 0xd9d4e7401e19a41ebb5cf270bd19ff91da71ea90      1
    216 0xdd323dc715f645feadd436b477a2613e4cabc573      1
    217 0xddf32cb1b9165fe3a452b56835d4804aaaed9aeb      1
    218 0xdf25c3ce7d37858922b1894abe131e6d65c0b4a0      1
    219 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    220 0xe20fb5ae209a1eaebaf6cfb50be12e528b0e0d84      1
    221 0xe2f7c6cf5cce07221c51eb4ae02b64aa3e518f46      1
    222 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    223 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    224 0xe3afc6b73b3e2eb36186f2e4bbc01b8aff5cb773      1
    225 0xe41e1256ec170575d61f4dfa5a21b190ac2eff1d      1
    226 0xe42ff1635f30213740da515ea55a9fbceacdf490      1
    227 0xe6517d239848fccd71dd8a4d301c360ed595efbb      1
    228 0xe6563194f9baa6e3064086a98bf73608fc17cbfd      1
    229 0xe9e794eeb8d6b6c04f4c98dcd28e360d9ed22f27      1
    230 0xef1382202ebdd0755bbc3754e14f07060ff991ef      1
    231 0xf0271659ae80147194e912283c7551b9175bdeda      1
    232 0xf220e57463860bc76679c9fc535f61d6cbaa6b65      1
    233 0xf2b24febe2330bc9186a7723c160e97dc9fbc8d2      1
    234 0xf2c452547004ede3c28384917924032c4904b3d5      1
    235 0xf39df112ea070c93f2841650805fde216218a572      1
    236 0xf3fe032f8345dc0f907952b39674e907720c8530      1
    237 0xf5c66081b4556db7781e06c905905baf89197a49      1
    238 0xf6014b99552085140b9619007ab91ea7b0b74015      1
    239 0xf637e64875e767c00dc2267f7a5fa2ff33531911      1
    240 0xf643ceed81114a3dd8f21b852679270bfd0c301d      1
    241 0xf6ddf9dafb7b2627ad00d14d9b7e20ce4a5d2274      1
    242 0xf7db96a0977f626dab2701c50a2a65920300e6d4      1
    243 0xf917f1e819946eda0b1587ff7c6ac17c38d117c3      1
    244 0xf92dcbdf576973c02c8c0543f253657877ee1ba1      1
    245 0xf993d5474cd607e26b57e1de1556bee36de2d0e9      1
    246 0xf9f7f745eecc8142cd557b45e34cba28234b5847      1
    247 0xfafe90e1a2518ab3cdc15ef44d76acd94b889274      1
    248 0xfb4c65f1dd92ae419f8d52e0ed3d94775476b900      1
    249 0xfc47f9776925bef108beeebc441b6baefd49f39e      1
    250 0xfe0547d623170b5a1c2c1e19124971d215b03acc      1

## Allow Artist Phase 2

``` r
c(allow_hackatao_2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01cd89226ba13b5e2000b5397dd87a1e73c81519      1
      2 0x07b967203be5630a09ada85124753de8a58f7fb6      1
      3 0x09309b99f3b43648734caeeaa752f77c679a2918      1
      4 0x0aa78b747dfc137ef96b428951ebe32f9ac4efa3      1
      5 0x0e39f72eed86c71135bc8a913d4ae1d7c2ab8ccf      1
      6 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
      7 0x109ad81ca063fa1a8237189e31821d3114c52a4e      1
      8 0x10e0f33a4ff62afcbc0a110e5b2f3c3b5a0e63c1      1
      9 0x127fa218c41ec1147f4dce71b056d03bbff0154e      1
     10 0x17124e15738b30d0abd345900f0a98049c6d55df      1
     11 0x198ca1beacf70b82c4e6de21e8cb7b6da6d8477a      1
     12 0x1b06efeee8b90fb682b5f55cfc72e01edabfa980      1
     13 0x1de346e7da150f066f7eea7e794d7d4ce0246ae3      1
     14 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     15 0x1eeed069282616f2870a298b4218a43277b7adda      1
     16 0x21e4c2c6691b79b108756b285510cce8a729d25a      1
     17 0x220f2cb01f3b95340b527adad03b8ba48fe96241      1
     18 0x230fced7feaed9dffc256b93b8f0c9195a743c89      1
     19 0x237a86eaed0df656d8bc4ae619760ade5b4705f7      1
     20 0x241d4a66de9d02bc3dc7df1046ce136a70fb5330      1
     21 0x2450273b90b28d47a969e72d66518ed3e131dc26      1
     22 0x2521e9a6c4639ff64d25de84c494de1b7b060426      1
     23 0x2522a2d3692bd3b7d1b087439318de38a1a6b257      1
     24 0x269288fae1bdcd81ad2fa5d6df7c594eb3197b88      1
     25 0x26e98637e30224ceb09c354289a5e12065009543      1
     26 0x2737c848bc712aa44b8a3fb10928ce9e2bb2a4c0      1
     27 0x275554f686c9d26c72fa5fc3e8baa0b1d06f9dc0      1
     28 0x2835241b4288f8752837e91b1ffd945b08cd8878      1
     29 0x2a26fb1a180a2ce5c6c2d06dc4d430c37e7ecf39      1
     30 0x2a69d12f42ebc78e40ff656c7cdc842ae4404c52      1
     31 0x303e662b11c5293fdba28c0aa13a66ec8c3bb7f3      1
     32 0x30d56626fc3438e7e22a8a8fc8ac2c367b28692b      1
     33 0x31f2792889c0bb79f7c9e8ded4f8c53c6c8f4765      1
     34 0x32085ab4c925279dbefb7e2ca6beca4ded01cf07      1
     35 0x35764fd659509f60c9531f27fdb5305f85a1398b      1
     36 0x36bdd1a3efbed8197bffeabe91815a84031375f8      1
     37 0x3805d1f0acd90867ea6700ce086145f31058b9ba      1
     38 0x3c2b022c60eacb8936521b88873e1f07b6206743      1
     39 0x3e6c0ef23ad0e323efbd5eb5c269b2acaacca1c1      1
     40 0x425e9d7398b37d31c6a6e024afbb303588b55c99      1
     41 0x43cf525d63987d17052d9891587bcfb9592c3ee2      1
     42 0x44b51387773ce3581156d9accb27849a204f31dc      1
     43 0x44f7ca6aadf154e9f10753240eb7cab3cf25c20f      1
     44 0x46d9cf4003617c62ba11e0fe1990997debebc88f      1
     45 0x46ddbc30eba19d767868696e88637e93799a7421      1
     46 0x48343da28fd9edda620390359ceef5c3418cd5ec      1
     47 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     48 0x4986bfb143e2afc58284e33253289b9dbb270957      1
     49 0x4a6e38fe4edacbb1030ae8d055bb7a6f90c116a1      1
     50 0x4be41038f789e527525ef5fe68777499ad9f0a30      1
     51 0x4dd2cddfa7139d32fc1c473b2701710a8caf28f3      1
     52 0x4f49c8f45d8b6c34e0c271bacdafe9564dc2a009      1
     53 0x4fc156fa2a598ecd8f92dcfbe4a9a2bb395bb234      1
     54 0x4fe76ce513915082f8bd450b6403295beae6a240      1
     55 0x52672fd481bd2593f81494ece7281e5396c18340      1
     56 0x52c57659ce57cf241797462bd06a26d7851cbfb2      1
     57 0x53fad1623bc32399dcabcc2164e57e1bc549593d      1
     58 0x5583402fe38c05b0a47f38a2e0f21a8c01919682      1
     59 0x56b5570aa20156dddb7e94e8af463a3617a88ff6      1
     60 0x57d864aa09a3334546ab9d3dfb5541aae8c30dc8      1
     61 0x5c038cb4a3bc5189670de3d01ea4bfdb58c917a5      1
     62 0x5c7c0fdc44b44673f070535897d1208104a28950      1
     63 0x5da7351a4cb03c33e11f51841bc614d985812821      1
     64 0x5f7062408d1a46518d88ae2713f7507b8b88c79f      1
     65 0x620e4e2f2573c8d10db9b16c7e8ca12742c010f7      1
     66 0x63c89c7212df4e3ffc8c6d9ed8014dd3a2d9fa5c      1
     67 0x647eb74a5135a0f24beee3c2d8432adcbb32c2a8      1
     68 0x651323817d479d40b49e2e63396f2ff47f225267      1
     69 0x65ab98d7ccd7d0542b008ca68c37bf14dc7f8331      1
     70 0x679d8c0039b68e19170aa5b0244e4dca6eafb38b      1
     71 0x692d1fce318f98865203a256b1024e070295c6f4      1
     72 0x69e3b90acad8ef5a40576a122d963b46a62f6939      1
     73 0x6a9d961e487f3ad2b0fce2fbb6487dc4b0fc9452      1
     74 0x6afbba8699e53d47bb986dda1dd39c73635cf73f      1
     75 0x6bfd30747ba72ac7bb2f20d6138e1312020fafe8      1
     76 0x6c166952052661810fc87def1ca6dfe4139101a9      1
     77 0x6c31ddb3d5a31b6d5d277b4754559576d12bf108      1
     78 0x6e0c6bf243232bcfef85a88eb61062626ffc1e2a      1
     79 0x6e53002eea4fb6d9b3ae1c3e30a5b6f4a487ab2a      1
     80 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
     81 0x703ce54ec26841ee568cfd6cf67a98060d998bca      1
     82 0x724cc4bdd17aaeff81afad42d3612d8a3d1979ac      1
     83 0x7309e85ae68377602747292358776fda3df70609      1
     84 0x734ed31ad4853ecb36e8554ec35408652642f3a0      1
     85 0x73667a14e46e6be3810ca9aa6c619c1c19c16b81      1
     86 0x7385211989ecd98f514493a087b94471a353bea9      1
     87 0x7442ae0096b7065d6de11f204f418b7ba1859317      1
     88 0x75b03fbfcd3d33cb27cf522abacd6476722a9428      1
     89 0x760389865b0958f705c350bccf8f14c5b2aa54c2      1
     90 0x7605e1458d5dbe8130964d5539e2113a24d82d47      1
     91 0x77350e1152efd5f2d807a6124015c629a907155e      1
     92 0x78a1450692a739396475ea201a312942f9fb0c2c      1
     93 0x78ffa27d122043874dbe879b7173180fed3ccf1d      1
     94 0x79b53c1f4ce3d969238861692183b024a0acb459      1
     95 0x79bb5016e87ec437be4b545f035ab9224a6e156d      1
     96 0x7a2848e79587aa05235f899840eef1308b1ba3da      1
     97 0x7a4068f43cd804f22b1ff8a5f3e38bff63a8ce8d      1
     98 0x7a601520558b1ca1d51dc253cb061268c5aa0d97      1
     99 0x7ac8882faca42db885424c1a0ff233020192ea37      1
    100 0x7ce438bf068c8f47f0f46cb7891fc7fd0956f117      1
    101 0x7d85806c589d9e1301898932a32108f15c6daf14      1
    102 0x7f902d8edd19a983e9b3eeb64c2fda2a19619782      1
    103 0x82c2ec33578dda0df06cd10e2ba46468e03e5e72      1
    104 0x8579784f87d5bdcf1d25ef2f7df1265a9462dbe6      1
    105 0x86fe9b4a3055994fb38175ef7bf3ecf88d0608d2      1
    106 0x8753982d800bbbf1faf478ef84c3e96f2775d3b9      1
    107 0x88bb8b2b32201fa3ba9fcfbb757ac029f9989610      1
    108 0x897145e0b4f90c86f2e3a7871c99d4efc5af862d      1
    109 0x8ac18b58594ee4fe81363031e929eab96892dfe2      1
    110 0x8ac65ff8600810e3470705cacb35a80fa218355a      1
    111 0x8c57e23cbf902ed01a7600fef1b8d083efb24909      1
    112 0x8cbbc94182fb53a3466731cf2c452e59ebd2608c      1
    113 0x8d309de16a745c15ffa00a46da4eb3e044c54d63      1
    114 0x8dea94ac7a9625e16195961059f88945aad80472      1
    115 0x8fccac516380d4c23a7b6b9aa3a6fe0f96bbc17e      1
    116 0x9247bf3aea57c37fb6ecd818bc40f34cc62aec6a      1
    117 0x92950e5f7169b2dfc923807ffd61de3cf0e8cb09      1
    118 0x9372c70a736e0c290a5c2717c70a9fe53dade890      1
    119 0x93e318478f731c8ba93f2d2b64f722319151e9fd      1
    120 0x94b7a1139d6f3d96d37c4f58be954ffdc2b65a22      1
    121 0x97e7124bbce0baf75a7c4688b98a0c1ddfafb3f5      1
    122 0x97fd53b64e81c1b8e295b6577a6c913fa442721a      1
    123 0x99f5f933beaadb5a0a970205c0a6c2f41c54dd8b      1
    124 0x9ae3f4135c35a69263e0bb5792006a2b76d5b2fd      1
    125 0x9bc50f4e431ddd9f732e734def1e581bb39ccf23      1
    126 0x9e4c4c818aaf38a6a8e099cb26093aba6c521f76      1
    127 0x9e69a931ccb27ac7a10192f34147c2ff0f861fb3      1
    128 0x9ec60b1557f9b66fc23f6e6a76988c866a812c39      1
    129 0x9f041fbbc6fd007115dae9bd1ce6001b26747797      1
    130 0xa0417f1696417a3541f04cc3aece3b56ba273829      1
    131 0xa175e701d4c92399262f590bfa92e3d041e61df1      1
    132 0xa1a638aabe66163fbdced5342193064bdb214295      1
    133 0xa1ad497e1780975dab7068977989160d071de523      1
    134 0xa3202d49590f4b71f734e47a99844a4c3ce9e202      1
    135 0xa359f9524a4986b5dc180fedfac9c0ed941b0615      1
    136 0xa639583dbee6f945c57492c63d8aeac64bd168d2      1
    137 0xa807de06cd55697f66c0b2c01fe77335f64ddc83      1
    138 0xa98fe33288c79d9cbb7d186d944fa0533230ba83      1
    139 0xac6379bd8e712bd23a21ffe2f37e920b91161075      1
    140 0xad803dd26508fec96e250d7ec541d58b77a8f5b1      1
    141 0xad991426a767fa52460c353f9b991b4ddc8bfdfa      1
    142 0xadbb28cfe191ce4393b279feeef9b5bc86603990      1
    143 0xaef48e7fb14048cc2438745165b6f72157d89cac      1
    144 0xafa955f6b0f0b929382781cfe397de8ed033a555      1
    145 0xafd2ed18640e4faf466b3658fcb81d18da5ea3fd      1
    146 0xafd7cb322e8b5f9b5083017d6f7f279d852de8c8      1
    147 0xb3479ac22ab13a9d359c1aa0fdf6f7e3d39a207c      1
    148 0xb5fc5a03014e84054c6c12ded379f5ae0ce89c43      1
    149 0xb6d67e7aa69aed737883ef83416e5a694e09c7e3      1
    150 0xb70cf8f26c3c8c038176239af2fc3f1ecd6bd3b6      1
    151 0xb78656e6f62a3e573163c038c6d5be9c10801a84      1
    152 0xb7b70c2be01400eb89c9f7cff667ae5685aede71      1
    153 0xb8f363e0442c23526431e85dd74bb1732c21bf8d      1
    154 0xb9dcd2f68174e1ed42714b1c928a7b574b119afa      1
    155 0xbff1205bfcbaa467cf39b213e1fa5ae5c256e571      1
    156 0xc0146c6536cb5b2ffd6af2cc49a70cd43a9a00d9      1
    157 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    158 0xc1871e12d132c37196e4d9c69beaa5c660576065      1
    159 0xc56da429ae8a1e8649016937730360e17580437c      1
    160 0xc74f1d90f1b51a4fc9c60310204804802e320025      1
    161 0xc7882f13c2087ced94dfe00710260f01fd812d50      1
    162 0xc9add0a3a24b6e87e9118e6b04ce86a8e1d9ee96      1
    163 0xcc44329a34109d973326edae0f04cf0f71f406d4      1
    164 0xceab2935e06c646e560e2f6083c55db6e8e12099      1
    165 0xd09b99fc927004d9dd7ad45a18dc9ab02eb6a7b6      1
    166 0xd5fab5d372081b4f20fd4636bde96e9061aaa8a4      1
    167 0xd636bfe79d1805a8d4da46e8d55d83457d309fea      1
    168 0xd6e1ac6ab670ab26fce65db0347819550b44e135      1
    169 0xd88b53b0aea09d07fb284dc4ce85daafb06472ea      1
    170 0xd8a55f364eda4ffdc9e9a5a33d1d2bc95e4674b1      1
    171 0xdc64332073c58ad4e2ff71c87883286024b086af      1
    172 0xdd0c246165c8b4a90bc5a446da9ee0908a45966f      1
    173 0xde2d411dd7c5ba3cd682e128a3e59a351ec814c5      1
    174 0xe1f1d3398d6dc28d627aa1fb3a6e8eac089613d5      1
    175 0xe6354edb3d71a675210eaf9e007f37b82a04ddf6      1
    176 0xe6d8796db42080df3bf047961d9cfb274865066e      1
    177 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    178 0xe7d254965c04468b559cbd796ee9d13b14011aa3      1
    179 0xe896675803da7df23c9bbda3646bbd82593b6668      1
    180 0xe90a403d8fb89d3b846d9c37457b52a841682c31      1
    181 0xea04c9ddd38cf475a8ccc6a9b31cd17a477f23b1      1
    182 0xeab8c38edf8295e70cbc97c51b3f78a3c3d3655a      1
    183 0xec9d209252fe238037021e6e6b1ac99cf7f46df6      1
    184 0xecf4d1b9bd29808a712a42ae64bad1b3f2c114e1      1
    185 0xef47c2d8fe59ebd029ae99494e7ddc484781005e      1
    186 0xf08f59b44c8bff72ea4cae15be3377791aadc342      1
    187 0xf201b83ab58b2e72cdcf8ffffd05fdc43032abee      1
    188 0xf4371c60608e186726ce9b3bc4621745ec5aa3d9      1
    189 0xf4c65b2bfb6faa0e0910ba668d42524b896eee59      1
    190 0xf5493d28b94521fe392f640aa78df3c68531964e      1
    191 0xf56b6f25fe293467880d62d804b2ab1389c38813      1
    192 0xf753b115835aa6bc3cab8a0e07a06441dcd5c137      1
    193 0xf7b3742e3a0af8f952d993d57405a3aba4565b53      1
    194 0xf90a677365dcce76c8c02a37d6eeeecddd6f17d9      1
    195 0xf974305604f874311735a17b34af435d7e498c98      1
    196 0xf9a7be43fc1e8e67f906c403e75cf4b21c93b050      1
    197 0xfa36c34701967c51c4ba43030d4f498c1fcd4d9c      1
    198 0xfa9784a7b7c9c6226dd2a3adfd9cc9d88cd78a0c      1
    199 0xfba5bf5c85fb2a51a38df63464b490bc8fb4d1b3      1
    200 0xfdb4fdc4eb9f7158b9cae46f78357f3cbc8107c3      1

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
