
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:12112       Length:12112       Min.   :1   Length:12112      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:12112      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18054469 # https://etherscan.io/block/18054469
block_hash <- "0x767d6ca5c677cb92320f1d13ed2c8398a619db92e6ea1ebc8400b188f5801538"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4565 

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

allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
```

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random300Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00172a79a73b9dfd5871a92a0365280186f7eff4      1
      2 0x0155e31d9c01c28395277bd4210bf5c51965e8d6      1
      3 0x02009370ff755704e9acbd96042c1ab832d6067e      1
      4 0x02ac3a098a3f8197a6d4a5e6fcd4114df6b3e490      1
      5 0x04a966a3033cd0a2f2e14adc660cb16deee2be40      1
      6 0x05feef6f10fe8890b82c9bf5065d58fb9b9cb284      1
      7 0x073d260eef1a0299bc04d82ceb5501a492e08f6c      1
      8 0x07b24ba3e50be7b4411138823176f4382163d59a      1
      9 0x084d2c0cadddc5233d1288255018beffd8f0df23      1
     10 0x087e14b18e87367a7cc58f04989d749b8a0e71a1      1
     11 0x09e342097e3107d5cf94ed60380c46ae8b0325ce      1
     12 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
     13 0x0b071354be55d703b46f20e279853ca8f4e28ee4      1
     14 0x0b976ac2a8c8b9d11cd6fffa688e6756379322c0      1
     15 0x0c973219372daabc365e3b97e453a500c482264e      1
     16 0x0e0b904e2940a31576326d2f335564b3c85a8afe      1
     17 0x10ad157640b203830bcc4164d2b12fbb03b266f8      1
     18 0x10ec3e9f9cfbad272e7596dffbc3466a6ae6e225      1
     19 0x110886b13e4a932396c0e9d53bf887770ca16784      1
     20 0x1246191c1bbcde42c1ee83e3745ba8ab37478827      1
     21 0x12b520c229886495fb88da799a285194c23ddbb5      1
     22 0x13a8cf48bebd5514031abb9b2993b7fe72e9c10b      1
     23 0x13bc4cc06e15abc085fbe05140fe9a0ecef0e6d2      1
     24 0x14b76991c8ecda8cbe4a5d330df992880a986fd0      1
     25 0x159968fad6a3df074cfe7e587cde0c5f375c1b70      1
     26 0x177751396d8236569c5c7b04232c7b7281a3b9f3      1
     27 0x18e804e61c44fd194753ce89b32a87acd8435a8d      1
     28 0x1a775079fde189642ec93829e298148813274bff      1
     29 0x1ad4fb4852d257987fd43613febe787f5535af47      1
     30 0x1b5dc7f6a12471bd4f6f584e648edd8fcde50512      1
     31 0x1ff236fac1158c7712beb01ec87d7fccc65e6dd6      1
     32 0x200b8319c8e962326771fc557d3c3e77063656eb      1
     33 0x22aa3f5d1daffe1a9df298e79a0cf2f98c1b92ff      1
     34 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     35 0x22d785f2558945a4c2aa6946771eb16fd1689bdd      1
     36 0x238edab30142a77ff1be2ce3ec42d5608d283434      1
     37 0x23e7441aaf7eef572370929971cab84e753579de      1
     38 0x24055d456255f02e5b6287efe2d4111900e69d39      1
     39 0x25353cc20c62b03d02c12025359eaef80adf219b      1
     40 0x25b12eea057708adcb521ef7da0d4112523372fa      1
     41 0x25cac2725f4964efabcb875976dfbd9eb932991b      1
     42 0x26b0f2d8c16e0076d9c00ff68bda5e75563bd215      1
     43 0x274c2c8fe00f8e50578e2fd8e0be5aa11f84fd84      1
     44 0x2762c7fe932b8688508fff24ececbbca92a128c4      1
     45 0x27e37590cc23bbaf90a53cf2e1afa70c285c1fbf      1
     46 0x2a86e9f48cc7aa97d534a2a3661d7b5758f20335      1
     47 0x2bfe66759f0331066f3e7d57b3dc9a96bfc17927      1
     48 0x2cd29cbd03d7d8ca5d9bac1e5bd8742ea5037bae      1
     49 0x2d2767ecff71d594823ef329a82ddb9f318cfe4f      1
     50 0x2d5313b5392f2709f4d1bed6f5f01ec0badc18c3      1
     51 0x2de5ec82bdb8bc7662e9994fb24ea6e6f79d9d69      1
     52 0x2e08dab3417c6825f90c1fe07b5fa1b8753a9ca5      1
     53 0x2e7d0d8e1912eae8243c557bb66196fd1c8577b5      1
     54 0x2eebe34227a023035db26421af47a2d48daf6ca1      1
     55 0x2f80b028a6e5c0d1fa92f62c3ed000af5bae143c      1
     56 0x315562f0dfbdfc2040242fc45b4ad1cae8c254e2      1
     57 0x31b23128eb1063faf10ddb10e773aea0a50db236      1
     58 0x32d3e3126ad99e48ff769fddc3e26a528b8c2c02      1
     59 0x33608f433eb5d007abcf0daf01ff687e2148cdb9      1
     60 0x336734a67020ee0f36fbdeb32517a5d8751bd0c2      1
     61 0x340b9239365b75bfb7585d26597f58d2f2025e32      1
     62 0x3433faf996a75d642a1e8dc82b8d70381e252881      1
     63 0x35c1993379e9dcb3b1c152b2d7116dfcd373def4      1
     64 0x3659aa07f21b361a3955f85bf7006bde904171a8      1
     65 0x36885851f71f09a087039260452e4756af87d9ef      1
     66 0x36c246d67dbd8c5c2988091bd6626440db5424d2      1
     67 0x37dcc02b6a1a56d50a62f584c8c39ff486c1117b      1
     68 0x37feeac37afa67d5211937b4fca166f91724ae80      1
     69 0x38090615458c10d067dc19b5f1ccf7d4b4dd8bd6      1
     70 0x3861dd49aa50ca22c61bc0e3bb0f3d597cc15232      1
     71 0x38f1dfdcaf2f0d70c29d4af6a4aa9e920efe8b18      1
     72 0x39fbe769de5c86daba06d3ecf889f2812b8038a8      1
     73 0x3a89cd7d1cf3eb657f70954f0011548675c7132e      1
     74 0x3ad3030e9623e9e09ae21cd0632aaa62446a5c97      1
     75 0x3b402b7d44b647367686f20b9905cf4a6eb6349b      1
     76 0x3e5cbba78dd36f651cefe21951de8f153ab1f8d2      1
     77 0x3ec7fc75cb0abf11d65a50f389061c3ff3463655      1
     78 0x414723d4f305f1dcd71d72bacbb9988f7625ebb7      1
     79 0x419d8dcd1bedcd2b843a6987515b986a67a58088      1
     80 0x41cdf42b4d22b3edfea997a2f03f32b156b135a0      1
     81 0x42377a55f958e2125701fd92df42aa0d274ca6c1      1
     82 0x45f2d5c2ae9042d6b621fad10f0d0734b29681ec      1
     83 0x47279fc734466ee8acfa50aed553ff79770c8779      1
     84 0x47a266e4dc05f69f4e6166a013c300f7af5ca1de      1
     85 0x47a366519eaf385804a404697eb8d5cb1fd55d8b      1
     86 0x487fb33529015dcb0ef85f40f48b42818f97e1ae      1
     87 0x48af2bd40905f8d48d187ecb3c6bbc19ec21c795      1
     88 0x4934fe0805da9b46ae86bc6008fc4e6f4984134a      1
     89 0x4b37c9a6670b58fa7fe5ca6ea4d56c7e9d1bc245      1
     90 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
     91 0x4b4446723612db79e7f5551ea0568248a0dd8343      1
     92 0x4c6011d5c53f7595095374c610c294f16a50e87a      1
     93 0x4c9b3b18a54e59b23a75250425670e6a7467d225      1
     94 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     95 0x4d6758ba3561a39ad29507363edff464303b4695      1
     96 0x4d8eb365bc03225e74bfb90ada1c2ada2b6cb8cf      1
     97 0x4e3a10f142075389848ba955ab6282d9338839de      1
     98 0x4e948c19640c00fa1d2538ad3e9da14ab7320a14      1
     99 0x5011a360b3d308644c4d423b43eedeaf9543ace4      1
    100 0x5054a665b7ac3c30939b02acb34827af25aba35d      1
    101 0x50b3aee4aadaf115c4274bda7b788572d859a9b2      1
    102 0x5154dc7b887fa42c9cc63631c6068b2ef9b436f3      1
    103 0x51a8725dc626d4cbc38936db547d9dcf395984dd      1
    104 0x51fad37dad97121bc30d166577189a0d771a0361      1
    105 0x52a98cc2f1c17acd12054e891213ae17d05e695d      1
    106 0x542fce2f47cbbbba00ba59f844b8e1aaaed1f84d      1
    107 0x5467aa7f330356e7628a7a16d4bc1c5891081c4e      1
    108 0x5489a663225fee3aa5bb8426f388bafa3ff3482c      1
    109 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
    110 0x5546fecb516d0a342107a5324666b07f73bbf707      1
    111 0x555a5083c82ca88cd4b4c2d0941495c9198ce6b8      1
    112 0x55769bdc7ec060d7cbe8d145fdd1f941782d0a34      1
    113 0x56879fdf9792ae08e890528815a317ae53bb6ddb      1
    114 0x56a061f4ef706e1f6dcdbcf7e10e4340c1d99bbd      1
    115 0x575dfa1096ac4cb85f5f1b15bb7cfd9734d8a064      1
    116 0x5972115498096052e3f3165ad7788d49935a6c6d      1
    117 0x59a6febeafd5faf543033377b879d0f9f513f52d      1
    118 0x5a46c8d3845d820ae5b69cf7081c5b7c01ed2635      1
    119 0x5a7bcfb9390b37fa0055e434b041f20a719e1aa1      1
    120 0x5ab2d1f5069dd2f9aeec3b0a8e923b1cdbe7fc44      1
    121 0x5b96cc343f3f8521748da96584f4a0be07b52b0f      1
    122 0x5bbb102049e1205a2a31901a377c7252b194b294      1
    123 0x5bc1fd44cc493280851d6f218f19965d4bcf5646      1
    124 0x5c0f520a980197543d847e6719457c6990eed1bb      1
    125 0x5d7a182082c433090596e2ed57d4f88cf6468d81      1
    126 0x5df5078ae2e65f05874e57929ca50e0ac689b378      1
    127 0x5fae9d4b591f213b3ba75287f2cfac0883d17f7a      1
    128 0x5fbe0ae423f1767028f150aa92545267507588ef      1
    129 0x61b4468832a1a78876e5c6f1797b2da9360c73d5      1
    130 0x62251103308a69be7c27d22f81e4b2dfbe00c7cf      1
    131 0x622d4d42d78be6e5d081562d3c4cf12f011798dd      1
    132 0x64036879f71a2448cfa629090e83876c5d72a16d      1
    133 0x64ac2084601f8cf846f359ffd956adb5a34c6722      1
    134 0x6590ceaf9227887c43c6b3eeffab08f326a1aabe      1
    135 0x65cb2108eb9f1dd127bdff5eac452f971bd21cb5      1
    136 0x67052b837563a6d19134f06c5ac2fee9f7eebd26      1
    137 0x69419b29e25dfcb0f1f27906cadfa332fc145d7f      1
    138 0x6a045f224e69a9f2ec95ba6f8a83afe03af8754e      1
    139 0x6a6d335d1ca6c4539b7cee6476b706685f97bc78      1
    140 0x6ba59d08d66dd37d550781d22a71876267c05e37      1
    141 0x6cb9275b70a362d8fe5d445268e92186d61ac8bb      1
    142 0x6db2374493a115f9a051fbe77d2963f2dd08e71f      1
    143 0x6e3258ce4ecbb70e3cb18692d24e82b675d2e2cf      1
    144 0x7051d33aadadb8c7f83bc3d7efbb015cef3257df      1
    145 0x73010bac31886ce742d2a8283321ac73d193bce0      1
    146 0x73a80f56e3c82ec03734827d149c76086ca85420      1
    147 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
    148 0x77297ab28bc330c345b5d41146c504bc2a2a54e4      1
    149 0x772ad5c5f17c8f1175657b7444553e38152162b9      1
    150 0x777c87cacd482f5f5667859bc0a83acc7c931151      1
    151 0x787ce8184512a78bb3d4dcb02e473eef4e0faa40      1
    152 0x78ff20aac0d76c3e01580f9181ad924f2b0e85e5      1
    153 0x793a68e64214a1c4a0406a214874b60781320596      1
    154 0x7c8d3c250acc8d827ef50cad15a8588cd652f9dc      1
    155 0x7d60a758f47822ec591b24df5a9d58fac84cb07e      1
    156 0x7d87e23d7777d4829845f03f31721e10775799fd      1
    157 0x7dc166391edba45b124e236cd099fdb266e541ff      1
    158 0x7f23534387ffbd9c5fff4868c34be54c69c684cb      1
    159 0x7f47a6f4b6b1b621b04aa1cab4bd75e17cc7e0b6      1
    160 0x7fd29e547dc2d2ec3773457295a98893a0db2e05      1
    161 0x806e37b374e4d867da45b24d8eb7584ae90fb2f5      1
    162 0x80bcbfebf7c179929c93bcd1c69432704427fb5c      1
    163 0x810a8c0bca2df39859c590bbe40777c419395c79      1
    164 0x81a80a99c8ab3c57823d6235134979e8c13b2239      1
    165 0x8215643e5757d44940586aa670c932bd703034f3      1
    166 0x828fe9cfa3c1d7ec49b7bc70ffe8898970b25088      1
    167 0x82f724dd0547f1962bf062539c33693154cae713      1
    168 0x8396a631492c60696ba4119c54e0fa616b2ffbb2      1
    169 0x83e8708aba59195bbb2c7fcaa83af1c536d37b25      1
    170 0x841120ff6d1e9f29e4ffbdb50eda696bbfd98ef6      1
    171 0x8451b49f03c80b8d4d116090ddf10047cd173d55      1
    172 0x8537b95ede7549c3341c4debc02215551902a65a      1
    173 0x855ab77e437a45d814b7b8bb8b8be38334fcdc4b      1
    174 0x8605cf36085697fcda20be5488df08f407f73430      1
    175 0x86fe9b4a3055994fb38175ef7bf3ecf88d0608d2      1
    176 0x8741e2c8220e8d5e9e698045be1af5c43124c65d      1
    177 0x87d8dcef48c9632e87de450e55941d68db0b1463      1
    178 0x885cb1217070bc7e05661a6b2a906ce3d9cea705      1
    179 0x888712f0397e830c9463f443f63eb8992841020d      1
    180 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    181 0x8c3996617922c3ca288958c233141d2672b6c4cf      1
    182 0x8c47286ffca3d75cc3b15ffe11093abe01913a3a      1
    183 0x8cda7294507d985aae2d7a168d51fc675ee1233f      1
    184 0x9116729bb16b97467656c63e1578364de2798e22      1
    185 0x91b49b932dd4b88523a390de9c4b964d463376a0      1
    186 0x91cb7aae3905c8de09c88592702c8c313d4e2109      1
    187 0x9263ede893f48ebbbfc3a31d356243c710b4ccb3      1
    188 0x92730185034d49ce10f0b0670f2ee3857415f178      1
    189 0x928ee99d063ba6839c30e663822c8cf6c6dcda02      1
    190 0x92a7bd65c8b2a9c9d98be8eaa92de46d1fbdefaf      1
    191 0x92a81515155de08443aef5a11a17501da22d8801      1
    192 0x92ff64df0d05cbfde1913cff17a531f068ea0672      1
    193 0x955d7584aabe835e898151294de93b1fd97d00f6      1
    194 0x95f1422062f660fbdab6da626799389d86b8448c      1
    195 0x9657a105aa24f8c0664aaf4e59fd6ec0c74e4a48      1
    196 0x98dc82470cd96449eff7a21837644e8a0b83f40e      1
    197 0x9d2fd63c49509e82c7abe56d864deabf41ff4a1e      1
    198 0x9ec11f051e8cf2b3a4ae9c8d93e8729cc3b966b4      1
    199 0xa061fbfa7dc7ee9f838a717e8b55fbc34641bf6e      1
    200 0xa0ea96c0201b3d567bb29cded320cff8f1e25fe6      1
    201 0xa208a0e5a1a74e6b9c3f053be2d1ea884067b925      1
    202 0xa2321ce998844cac8a2f67a481fa3e23e4c6f039      1
    203 0xa26e13cfc426d42480bec7874e27cd1e1089e32d      1
    204 0xa36cbf2a4488e43a432a38807a7a4b1c21ea11b1      1
    205 0xa36d30fc6fd9b6e23e2285474cdcb9096a72840c      1
    206 0xa412daa017927c717887ed27aefa0b720ae25b42      1
    207 0xa4f0670468dfb825e6c2b5571e2c97d0844190a3      1
    208 0xa54e54567c001f3d9f1259665d4e93de8a151a5e      1
    209 0xa56131d1c79a35efeedd812bb521b81c6712e407      1
    210 0xa626d27bbe486fa00f58df20a881c7ad224c411d      1
    211 0xa6c12d417553f4c9a12c6d4376bc2b56e43eb2dc      1
    212 0xa6d738f0ddf963fa9d4e5952224cab2ce7400753      1
    213 0xa7c3f481810149f3853993abc4e85c49023cd2b6      1
    214 0xa7e41ebd77d94b5dcc5e95b66b694f575dffd3c8      1
    215 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    216 0xa84c6f7092d51389f014168ce9641122809b4bf6      1
    217 0xaaedd94c92e7079e22786e5931235622b68520b4      1
    218 0xabf107de3e01c7c257e64e0a18d60a733aad395d      1
    219 0xad887834294a92d5d6590608de07002be6fa3384      1
    220 0xae805b9eaa5a04f307b439484ed881e75d36ac73      1
    221 0xb1736ebbdb2f0bccbe844db124524457dd8284de      1
    222 0xb27f9f558695ebe11a54546a055411bb7464df41      1
    223 0xb3276a67634206590d3e0f7192b4df34ec184eb9      1
    224 0xb812b11e5bfbeda4f33da7d96ecb9bcaf8276d71      1
    225 0xb9ef0e8383eda06ff12b274b961febb0cc97840e      1
    226 0xba3e6e7918f2b767930294545988b8143257518b      1
    227 0xba64444d6d16d3661644464140d2dec3708ad332      1
    228 0xba64801e510f7f334ef947c668a830cffe95916a      1
    229 0xbb033b792b7153711bcdf8c9d3cf0f0e28896995      1
    230 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    231 0xbbc56aa6b885e28d4822677f6c25d76cc2856c2d      1
    232 0xbd2c9b8e92eb619113456b2db3ca27e935856247      1
    233 0xbd6006b93f10b484d22d5a9d4e17bfd0ade4f614      1
    234 0xbee1500a2fcab995558cf0cca86b986b06f7cce4      1
    235 0xbf9d462c9c6880d44001bcb8ca4a38a351d583d3      1
    236 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
    237 0xc385caee082bb0e900bccbbec8bb2fe650369ecb      1
    238 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    239 0xc4c4aed3871be9ebe51316e2ac888635ff633106      1
    240 0xc4e19acf82441eab7e45a5e874cece70cd811434      1
    241 0xc575343fc91828440620c0c9a2faab09d26f4f83      1
    242 0xc68bc160afb3ed468e0d98ff645ec760f6fa5669      1
    243 0xc6ce3202b4c5da7db2736e92383f6405afb57715      1
    244 0xc73c4eae45adf120a37075492e0ad9971a5c0207      1
    245 0xc7db4c95e3050c73db2da4041a03c24df14fd024      1
    246 0xc9a1a9caaa608dd3a7bb90b5f16b4a0991095aef      1
    247 0xcb25b216e4241535fe0e36d5f83f36a4a30b233e      1
    248 0xcc44329a34109d973326edae0f04cf0f71f406d4      1
    249 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    250 0xcd733fea07b1b68a63b6ffa84ce0d12a94f0bc22      1
    251 0xce378db0fc9e1babd15132ecea53809d572f5664      1
    252 0xd16091bbd6bb0f76241de83e4d496e1773089e33      1
    253 0xd1f0c92bd9b149b5681ff2ef2213e4322033d0b8      1
    254 0xd27fc8aee201ec6a35f3ae32c457d04c44965158      1
    255 0xd646a3cecfd4713095e0951a6dc939886576d9ba      1
    256 0xd6a234a57a3705a67dc67c6fff9f750df690ce22      1
    257 0xd76d392d17379c22ca5e49d167bad1dcaf7eba0d      1
    258 0xd809a687e957761872b440909348fa6547cafabf      1
    259 0xd8cde9b0bba2ce08eaf8ca7198156c08a5451cb3      1
    260 0xd8e93ccf41f079627f40573152bea5178041e1be      1
    261 0xd987dfaee54fee1045983c643edd0c13304b1d70      1
    262 0xd9ba239a881f718072e57d9810846c8d705f93a4      1
    263 0xda8b6639bb38a56057ef26a92e2c7580f45fc5f6      1
    264 0xdaac4e26f11223cc043dfd8e3efaf72333067339      1
    265 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    266 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
    267 0xdba5e808593f9438963c0414a225ceaef40ca1e5      1
    268 0xdc7bfc3058654da90692457f68ee3a34e2dd3908      1
    269 0xded440a3b8d76c828db6737c4368ecbd374c2237      1
    270 0xdf4329a7fe5b265ee0fd7315432bfed7498157b7      1
    271 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    272 0xdffb2d60e5ecb233d448239988d3a6fb6470a8d6      1
    273 0xdffd933cead5eac96397a0c290c5dd715366bad5      1
    274 0xe06f9f90c459031d5d7f612ea12899d1f2649979      1
    275 0xe1f0da1b238db9cb3e51bca447f1210c7134277d      1
    276 0xe2c945b5093f6314e889e0c5b260d86457c47301      1
    277 0xe33db7fd452e40c93f25a0ed2f7e5724414bd255      1
    278 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    279 0xe4cbda373d6a445f83fc9125c6230ba58bc08320      1
    280 0xe6e4d92009406d08851c2e65ce6dd324ad76a87e      1
    281 0xe6e82ab45c96fe92d050925a39f91449a7317f7a      1
    282 0xe77a08d1513974f9cc89fd2eb043eb68355eed20      1
    283 0xe8f19812dea9a836125bec88ed5ad9c3436dd3fb      1
    284 0xea6a4c5514d847c1a474a7b61d305b7ec11f1373      1
    285 0xebd56f0884a8ddb3ad47c767c5647738b04ed1ec      1
    286 0xeeb2abcf0ba8f51fbf6cbaf357d4193c105381c0      1
    287 0xf1495dfc8cf795bdace6eb5559520e91d872ebed      1
    288 0xf4a11c01e5e8244233c5a4e7ff4052781c333e36      1
    289 0xf5653e48e24e04cc1b14131ab31b776ae8718b34      1
    290 0xf5731ba57f5648562d159e912cbc2e921c8cd5d5      1
    291 0xf73f3f426b14fb4f0f62e14429960f23f42e09ae      1
    292 0xf7d583bee22b514a4c4dfea147524642a722115c      1
    293 0xf87e9924a11d9595c415ba0eefb2e3088a04436c      1
    294 0xf8ad3f88b0e0d177aa8c5e6be1e13410fd41cdc7      1
    295 0xf92dcbdf576973c02c8c0543f253657877ee1ba1      1
    296 0xfa75e74d9c12bf8c7844101c06614f1d87e8eaf4      1
    297 0xfae34e63ecbfdff0d6d14c5ada3268c5ed746c12      1
    298 0xfb1833fc48c9511c19d3c5f12e86f7ed28993bc1      1
    299 0xfc3c6a609518116be8aa89b2e9e9dedfd0555c62      1
    300 0xfd5d8237da011a140949eb23517fbbb9edf142ce      1

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