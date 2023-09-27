
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:11516       Length:11516       Min.   :1   Length:11516      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:11516      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18218469 # https://etherscan.io/block/18218469
block_hash <- "0xb102db0f16d1df10ef0d9bed42bc407092c5cd7da6894c3b16ffbfd5015dcccd"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5005 

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
      1 0x021767dd74beef783fc28ad858048cdd9ed7e3bb      1
      2 0x027cae2ed1a23350a751452e907b4120330f9762      1
      3 0x03b367733f46743534c9bb13bd4fb5ae2780babc      1
      4 0x04547314afabd1060fe6513518584b1ca602b51c      1
      5 0x04aef0c8306c1ecb8cf17ff152c9b62e01282c1b      1
      6 0x062164d0c571002b9b34a9a71ddaeeb0b6f93132      1
      7 0x062f25c17e026104d8e5d4dc15e3fc70d4a51d24      1
      8 0x06c888f9d273dc38e962306e3b3ee590c776f7e9      1
      9 0x076d40d4453a72a3dec44970b1529baee379dd0b      1
     10 0x077be47506aba13f54b20850fd47d1cea69d84a5      1
     11 0x08632358d3f5781cb2bae28da286aaf4ccacaaa0      1
     12 0x08882cd344a56c3da51a19def6cdcc9b290e7c80      1
     13 0x095283c4a52252599e45e89629819013512fb4ad      1
     14 0x0961c9a38a8d51b7cd43466ad3e88a3b84642661      1
     15 0x0a5b7c226ee4c3510a6f567a5e081971a0f79a90      1
     16 0x0abd672a6f2a3e2907ee3d8ac9744670c04df8e2      1
     17 0x0ca3a3b144148642f68b88bded521ce637db8f3e      1
     18 0x0cf7e52aafec849a4df31054168bc2b1a39bc316      1
     19 0x0e10d178333bd38285e0f12109757ce34de59acd      1
     20 0x105965f34e29f55efade78337e3eea727faa8f67      1
     21 0x12dbcaf089d921da60fe5dfaef912f11633b7dc7      1
     22 0x133fc918b3a27fa314c45a08ec7eb794ef0283fc      1
     23 0x13e4c8c584f3f7b43454fe0057cd88059bb39cf1      1
     24 0x14c3681d58b4f1c17735b2c7f90a8463aa51eec7      1
     25 0x15bd2cbaa3e06b4fe9c1f7938521c63a42fe5328      1
     26 0x1660207bf5681c9cdb8afe3a16c03a497a438753      1
     27 0x172cff9b843d0ae167d33a74709b0a3affb6a5d1      1
     28 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     29 0x17b2d1911262b9cda80aa5354a1c13ee9027ace1      1
     30 0x1a63faeeaa780c1dad17c5fd1459b958c0ba1b92      1
     31 0x1a775079fde189642ec93829e298148813274bff      1
     32 0x1b286518f6ae3eda6111f0bf13d3409e2e5b9e94      1
     33 0x1b59628a95a55e374b8ff4274b4e88372bae9484      1
     34 0x1cb3acfd25eb1d1db28ae60cbb78bda352297266      1
     35 0x1e1500c0905fdaef033dc4c1fcaaba2a46491f4e      1
     36 0x1e2eb50a5ff17b4d2e143474206df72350375b22      1
     37 0x1e31c297fd3d88710c35e9e1d7eb5ce27494fc72      1
     38 0x1e38317b64e0ff5d3c500b3250afa2527357cc0f      1
     39 0x1e83bd0a719b6ab183563b174062ec293815da5f      1
     40 0x218cd7a6ecafc8dfb02456a61539fde5dbe5d22a      1
     41 0x24970b51dd29dc2dfa264cd51c41998c68e3d7e6      1
     42 0x24c99c4f527559d4fefa735f5732f0a478864057      1
     43 0x26ee1ea70a38fee396fc5db7d9d8c75b93dda0da      1
     44 0x2847c232662d53404529938e7c9357e34a5051d2      1
     45 0x2916768f1fea936b6c69830b8e1e3bad5e612255      1
     46 0x294af89e9c338f5f0b54bdfa7db81af97f6cd24b      1
     47 0x296c2d4daaef7b105ea4482a842e1cc882583547      1
     48 0x2a903ac2b09124a7a5ab7874050cb217c0f9cf3e      1
     49 0x2bd17fa766dcbc697c8fcb29f311da53fde0074f      1
     50 0x2c4a6fd81e52d256ec34db1e03a9af627b2d62c5      1
     51 0x2c66b04965688c92e4487570b829796d090363c4      1
     52 0x2c673dc2eefbf4c95597f3953a55861e38e3cc80      1
     53 0x2c6f446d354199c635be737508423b4686c8c751      1
     54 0x2c9d56f321fd120009743fef09e2a6110178e200      1
     55 0x2cae0ac9a7a7048516868aad672c49ab632b38c8      1
     56 0x2e769f9cb238e5d93fbe353831c80bc92ba239d6      1
     57 0x2e7d0d8e1912eae8243c557bb66196fd1c8577b5      1
     58 0x3030b2a5bfb7512972cc51f9389188608ed867f7      1
     59 0x30eea1e603b556eef164b4554bb6e7471b3e70f7      1
     60 0x310b73b2873f0be20ec08a401e649a27410b6295      1
     61 0x31222b4a8e0a2592d8ee19507186237faaef4467      1
     62 0x315562f0dfbdfc2040242fc45b4ad1cae8c254e2      1
     63 0x315d2c908f72a8d76f68e152699a1b407b557397      1
     64 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
     65 0x33bf10b2b4a57bc20d955c00b2f735897124785c      1
     66 0x34092ce00826da047acac0ddd0b366eda43bf0d3      1
     67 0x3457d2aaa857fe5c0574757c00ca8df5c0a19fe2      1
     68 0x3544311b3c5f21edcadc9f5515f3acd74f502fff      1
     69 0x3561364200e9d1c30bf5de4d75f2d97d3eab2441      1
     70 0x367da18c27e44c74d918a27a179b432de2bf119b      1
     71 0x37c1e3d3c305acd0fb2c38b88420867d26b5f630      1
     72 0x3804107dbabaf63d3b8e2b11fe43fa3caa811fe9      1
     73 0x3819660cb4d48b192b4973cb8323d6cb1404d930      1
     74 0x3854bf7264b0c4c871be8507fbf1de141bdc3b5a      1
     75 0x3932fb6453fe830eef57b77f483837ddd3fe3f3a      1
     76 0x3d29d165656135fb106529f5959834e024b5cecf      1
     77 0x3d45ec9d2fc1450079fe107e49ce5377e61fadd8      1
     78 0x3ebd392d8ac5c3c661b60c7bea129dcb1672b07d      1
     79 0x3ecd587a9dcd5e07546c5687bff83a040eb66b8a      1
     80 0x3f3dac9bb1458b644be33718c447128e9b6a87e4      1
     81 0x3fd7783b11d972801d2dccdf5609fb8842234c5d      1
     82 0x4072d2c6e58c9a3d43d692c0dba81ec0eeeed15d      1
     83 0x42617123b481f61df6b5b8a6484ba0a4e6929279      1
     84 0x443037c611e0236c0c3d1e811c7b785d1d360ce2      1
     85 0x4509624050a93d2f37a1afd444c07907b8031ab0      1
     86 0x45db9d3457c2cb05c4bfc7334a33cee6e19d508f      1
     87 0x461b826d3c6108a420ac696631dff5a6a425e01e      1
     88 0x46baccc803a49f6e49733d9e6b17ccfe1ff8f459      1
     89 0x47bceed9670a928ffa1540d048a4386ae11ec4eb      1
     90 0x4874211da929dd87e28a4267c4b96a5d0ccf48a7      1
     91 0x48f823fd7f5fd781ef2da1a1e53a92d0f45ca176      1
     92 0x4943cc7813da1697265fded90a2bfb749eba0d89      1
     93 0x49d18648e4aa35368a683c24881dc94225518751      1
     94 0x4a22528949fecf2bd2bf7364c83e389d1071ddd1      1
     95 0x4a2bc6f59948174714657735af944261ceddf67a      1
     96 0x4a4f1c998b89c0dc000983ffd997420dad282ca1      1
     97 0x4af694afac543c2586feb46a3fd2b3b9f3d94f7f      1
     98 0x4ba7a675b583cd228fb6ff23232c8dd41cd715de      1
     99 0x4bf7db76757302876d319ab727e26ab66753128c      1
    100 0x4e3a10f142075389848ba955ab6282d9338839de      1
    101 0x504e0e24e5cf5534606df3e064cb7faf49d11ba6      1
    102 0x5062643314417a88ae40bbdc8b15f21c4abb288b      1
    103 0x5154dc7b887fa42c9cc63631c6068b2ef9b436f3      1
    104 0x51fad37dad97121bc30d166577189a0d771a0361      1
    105 0x5467aa7f330356e7628a7a16d4bc1c5891081c4e      1
    106 0x547e56e9b748ba28d4ddd354da89521f9f80eeb5      1
    107 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
    108 0x5684d589fbc3ef36db91b228e1c6f950deb97749      1
    109 0x56a064ba6fbcb7a47cb570f9e8f690a68056a0fc      1
    110 0x575dfa1096ac4cb85f5f1b15bb7cfd9734d8a064      1
    111 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
    112 0x58cc72c8007eed0cd4840896838bfac3ff92d254      1
    113 0x590f4ca9b70860d1b89be8a7e69f22e59f6dcf6f      1
    114 0x5ab5844dd55ab73212d1527e4cf72fea884e39dd      1
    115 0x5ae2b8e7aab54dc0ffd571c30f76b2d8cc9fc1ac      1
    116 0x5bd906bc2c38ddf3abd65b6e81e5f2d2a4ca98ae      1
    117 0x5c0f520a980197543d847e6719457c6990eed1bb      1
    118 0x5c9032bd91b230214f14a94110589fadfc6b657c      1
    119 0x5d18c78f374286d1fa6b1880545bfad714c29273      1
    120 0x5d2035ebe68f4ce03d6822a68472d82c74af18cf      1
    121 0x5e7472223d5d28af97832934dcb9e5408953cefb      1
    122 0x5f5b5a6b4661c2a4280984b3921ffd3d58bd42f1      1
    123 0x5f883431d7467e42b1df641ae7b57144b01bbbfa      1
    124 0x61fcfd41c9031e5af6300da75b25f5f83ca2d647      1
    125 0x6430c11231ec6723e5bbe6ab258f184f5af7c836      1
    126 0x64d109081b38c4884708d816baf0dca36e75cb8f      1
    127 0x64f6cf1ed66fcaf299eedf5b63895bf0bf5ffa24      1
    128 0x660c3bf8d0f8b84a9e1e7a076cfe4685128f5f7c      1
    129 0x679bd1d460b29feed7bf989d1d1f2db0449eb605      1
    130 0x68a7ac13477aad590982293feeeb786a00276cf2      1
    131 0x68d503ffd7659694f36ca8fbcce21fbb47490472      1
    132 0x698b7dcb16b624f104b37a2578acaede89f37fc9      1
    133 0x6ae0209730fe05afcf9fbdab98e75dc9c7836ca9      1
    134 0x6cc4774cf4d4c738e3310f1b210c6ffe23d93999      1
    135 0x6ebe7f9cda8619ab2c508ba6c8f6675836185e0c      1
    136 0x6ec04c89f16804a17cc260866e8f7087cdeac433      1
    137 0x6ec79c81d230c29debed6f328cc27d108c96768b      1
    138 0x702782ec4d986dd1d821d71452cd6d10ea5e0ea0      1
    139 0x7051d33aadadb8c7f83bc3d7efbb015cef3257df      1
    140 0x70ed6061b74da6a290bc59f53987206a5dd93fb9      1
    141 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
    142 0x720ff4cc224b2d3f0cbb06834d6b595e6c3be84c      1
    143 0x723d5453fc08769cb454b5b95db106bf396c73b3      1
    144 0x72eac0495f5aa6f7471b8193a57747eaec8dfec1      1
    145 0x74a228c2e1f7d8f4b0a336bc024021853da8a696      1
    146 0x75182ab9bea2966bdf3eacbbc2cefba953474c65      1
    147 0x7537f404ae396bdbdf0e9d79f81940be55577466      1
    148 0x76417ac85338c705ee75465699b02e738578adc6      1
    149 0x76e6161f1fd846100140cbc111d0673b216a74c6      1
    150 0x77297ab28bc330c345b5d41146c504bc2a2a54e4      1
    151 0x7771b1d9967e528152f5db90fdedd8f6c64c6fc4      1
    152 0x77d24d4d990d439d0c086d93b932c175238e812f      1
    153 0x780193b15b2db9b9f4846dcef7ebfb3e2b751a8a      1
    154 0x78450ed7653901900d34273a768b23f77cbc2556      1
    155 0x78c74a90e80b4d93b911f979839cf764be00b4d7      1
    156 0x79473b5482ba8de8de5c1e2ad08113e1ec528951      1
    157 0x79d16570483afc8c749216bfc0b8abf759ead7c4      1
    158 0x7abdd18d37571e1ba884a8fa9e07a3612b584661      1
    159 0x7bf8bf27e3f300fece90eb785e4f09aba827edde      1
    160 0x7df2d73d80cfcfc876d180e4de52dee00e84ef1a      1
    161 0x7fb5cb066fea693bf39f9707b496514731232071      1
    162 0x7fd29e547dc2d2ec3773457295a98893a0db2e05      1
    163 0x82faab046e03236a09cec632fea6f172f69e44e2      1
    164 0x847affb5db7197fc6219cfb3d1b2404583978d75      1
    165 0x84b9cb209d28152103e7b03d54b286886ee05c67      1
    166 0x8772481791d3b8650ad0299f1a566085f1bc6f1a      1
    167 0x885869cb03bfa16e2e21e0e0d1b64b8ae9374f4d      1
    168 0x897e842a5f50978f5cd337fe41e6e37099239be9      1
    169 0x89b88287e2905003551f06ed77c3491be360de8d      1
    170 0x8a85fd123fb3d211b979346204772209b39e9af4      1
    171 0x8b0996bd9573299908b7114ff55b1fa96ad86423      1
    172 0x8b7d031fb0d8aab5a985583c6c56c4fffab27ee7      1
    173 0x8bbc693d042cea740e4ff01d7e0efb36110c36bf      1
    174 0x8d12daa8d907ad9c86c1c317e83cc430e9685771      1
    175 0x8eac39817aa7a1072c6e81859aff383f534a4105      1
    176 0x904759a3f9e848de36d271436fe922e63e3675aa      1
    177 0x9283099a29556fcf8fff5b2cea2d4f67cb7a7a8b      1
    178 0x93cf0a22a26895650a8aae960bf85a01ec6a551c      1
    179 0x93dc8de488504932db3bc900045afcf88fa79160      1
    180 0x94a5705a0f0927099638fb068c6e3ad77501d837      1
    181 0x94bd59027a36469827442108631d4a07d0e08846      1
    182 0x9572001a64ad0a42fb908429dfa0214a3c18079d      1
    183 0x9576638139843c636a0522f3bff3a85ac7b07172      1
    184 0x95b5e520e93e92954c6100272696b0e6cf8bc013      1
    185 0x95e40296bce5fe39dce227424a1b1640e8594b1e      1
    186 0x960ba434d2cfbc969c810c70613733c9ec226972      1
    187 0x96545a59ce81bb6acaf95957ea09cf577889112f      1
    188 0x96fba9250c7ddaef1cb4ec258c79e58f5aebac23      1
    189 0x9725267d94b029769d68a91ed8239a631b69cfdb      1
    190 0x988070d1060fd2c826ccbc9b1446296a96fa75ad      1
    191 0x98f9626154f44e3f30c112e9bb48b0678568b916      1
    192 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
    193 0x9aa824a302597e3477d0435cfc5b1e1b9eb23449      1
    194 0x9bf0555ce71d529a8f2d79fc4cfe9aabc2de21b2      1
    195 0x9eaa362844dbe545b63b8c0bda90fed9ea1f5625      1
    196 0x9ed249b910842e9913e3eb1187f784f1db6944d1      1
    197 0x9f432a2964351a7830bd9ea10f65107677d73e04      1
    198 0xa1c0c0ff6eaa63e7b0c7bf441fc146afdb08fc7b      1
    199 0xa2f8d521c8cf328132e187378d545b2b37be2f31      1
    200 0xa42fcb74efa879f9f27c1cd521bec8c76b324cb4      1
    201 0xa4c91f47457643b69fa7e743a75126c563e7acd2      1
    202 0xa5bd06a4722acd345b0bf872a0780f1e1db33a6d      1
    203 0xa5fc9436df21125e88a2f73089bd17dbafe46e74      1
    204 0xa6c579879252d8abb6e9150e4aa0196ba81c7b27      1
    205 0xa6e50c3d38b21a606f919d3eca0b6e9271704fce      1
    206 0xa77361ed1c7d78cec23d7437aeaa86fd0e284714      1
    207 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    208 0xa891fde42e982148b5f2f6b06f04be8e706a190e      1
    209 0xa8f3d8344d20029b667192b48156899a60415ed0      1
    210 0xaa20816a724c8bcd2b8bebb60b1a7a1f90e3ec0b      1
    211 0xaa300c7fc927345162a00b83fca3ebbc5828b5eb      1
    212 0xaa43b3ee536455939ac6155993351a9b34f72ced      1
    213 0xaa47a4ffc979363232c99b99fada0f2734b0aeee      1
    214 0xaa7b5fca32e51aa39b938fa2633ef0c74ab1950b      1
    215 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    216 0xabe01e6713511c6600b559c691c526a019d9f266      1
    217 0xac718fb27ec43925bbf905f74463236e7605cf70      1
    218 0xaccce5523ed582f41f3d21cb9e3a306c4489bb48      1
    219 0xad0f4b5bd7682aa7b87c94c4dc7671f37fe00c78      1
    220 0xad4002c4d4a34675a1887bd36e11a8372577a2f6      1
    221 0xae5ae13a008b662336ff3674d920819102ae4256      1
    222 0xae63171489b61bab3a3d5d039cf4bdd738845498      1
    223 0xaf37d6b9bfe0f5e4954ba1f2d0734aac32b2ae34      1
    224 0xaf803180a4e75eae08697b36fa0bbce292fd3949      1
    225 0xb0f96a681e2e1bcbf40f99f0e48b8f3108089cb2      1
    226 0xb235c9fc67e6d850a31035432bc2d200e350743d      1
    227 0xb33dba37a572861bf8d6d6237dd4fc048be5410e      1
    228 0xb547f7b9773839cff1eadd14f99928213346b101      1
    229 0xb5a4a9677de36140b4330a3c90500a091723da4b      1
    230 0xb66f89259545c7a323832833e8ab467297329b75      1
    231 0xb7560b71de74edbbda141ef1e2205da77ff53dd6      1
    232 0xb789221616c7be137f33d545758f5510591d725e      1
    233 0xb78e8334eaccea894bfc11eeed1514e1fb78abe1      1
    234 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    235 0xb92338347d4ce5b0b3d20d28985e458908f43d52      1
    236 0xb9cad3fa5b7beb0033ee8636c40c7c5755819af5      1
    237 0xbb315ba16c951cdccbab7be11b935387d82d98cc      1
    238 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
    239 0xbdae87fd1cbe5c94c38bb483838aba1767231290      1
    240 0xbf4c53721e3677a6c3d10e1ae08918d0477297ad      1
    241 0xbf5365c49a7821ea3dfd0e997aa547142706b159      1
    242 0xc00acc7010df6615354019e7ac6eab15d4ebe050      1
    243 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
    244 0xc087195a816e1f247f1865189d76c6be0aed9982      1
    245 0xc23485ae4315c409f0175a782841e0f10f12b0ac      1
    246 0xc264880fad20d56eb6c5c18bcb2f3d50f72c7663      1
    247 0xc3956f9e14e62bfe00672ac5a4b5d11f84d8b5b2      1
    248 0xc3f4728f673181e20862d21d028cfeaacfb6d409      1
    249 0xc73b8bc73a63c94fc7c5a54d026faf7b540cf113      1
    250 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
    251 0xc81bdadc8f4541a03296ac019fe830331cbca51f      1
    252 0xc87e9dff30a7472329237c4ad222b35b529f0fb7      1
    253 0xc93d22d9f4049447f94a0ae8bd25d2c2abee66ef      1
    254 0xcac725bef4f114f728cbcfd744a731c2a463c3fc      1
    255 0xcb90dd8a79be00ca9ab62284978cf881c2899346      1
    256 0xcca467c01aef7ef350a7fb2a3b6a3c78fb9fcf6e      1
    257 0xcdf4930c1e3167faa772fa463ae8ff44d61273f0      1
    258 0xceab2935e06c646e560e2f6083c55db6e8e12099      1
    259 0xcf9799f5292bf594d1716e941c67bf77c29e1a8b      1
    260 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    261 0xd13e4780ae2e4df44d4195a169ed1be557e8762b      1
    262 0xd18c33ef15f878dce373458a7da93da4f3f9ce0c      1
    263 0xd1c6d8127312ba8715db4a666e7cab1af898abfe      1
    264 0xd2f295776af09d33e0653cc4d0eb9aa716b4944b      1
    265 0xd2f75604358c867c459521c45c2e9c45fbbdc772      1
    266 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    267 0xd52fba65a52214a65c78dc7f95c9fee1f13a5955      1
    268 0xd5755a4276a53ee7ca2703c6e1967af59cbc9feb      1
    269 0xd5cb542f355bbb575ab61b57b799e3b0bc40b048      1
    270 0xd6701a92ee4e8df99a7ff1a453abba8da84e0c98      1
    271 0xda6bf1dc76d4f166472cf329ead6a19d19be72aa      1
    272 0xdb1576b1939edfb84c167d6fbc70bdf104634749      1
    273 0xe0b685bf93818ad568d67692dbb1967bdfcb9447      1
    274 0xe250c7540ffc0fe802a3cd1ba907adde9d410051      1
    275 0xe2bcab74729547a66c66a7d86b55b43e6506c347      1
    276 0xe4d50d9489a3ccf9d11c12720dbfb0f32bab0cff      1
    277 0xe55f32e874b540f6ad8584a56a484b76cda4a08f      1
    278 0xe6367b85401b4a5ba39248fb8a87a3e542ae088b      1
    279 0xe760bc1d8b3a8cda9ef23708bf10aa1c530119f6      1
    280 0xe971e45db57eb8ca8aa2bb2dc6604afd5ea1a1d6      1
    281 0xea72fbceda4e851c9171a6b57a287ef986344efc      1
    282 0xecbae8675df371a11724684aff5f81fff53db5a9      1
    283 0xed35a41508fc8cd2be39de2ae13aa72702393a38      1
    284 0xed6618cda6a25482b99fe8bc70f039034575ee7c      1
    285 0xee159c4fd8ea6edd8d4c6b7c64815e44feefca1a      1
    286 0xf063be86ef61e4e9b6baa6e6123838da32639096      1
    287 0xf0753b5515c095cdfce5a0d58af15dc5aa46fa94      1
    288 0xf0c3a1f1d9b79d5a8decb8e92973cdb56b7e81da      1
    289 0xf266bd362edec2656ff30d797206d1ca608178d1      1
    290 0xf30e9e77ccc9ce4732a2f44ed7a7888848c1b29a      1
    291 0xf416d2d8f2739ca7cf6d05e2b4ad509fafc42f78      1
    292 0xf46b076ff8ac8ae163fcd6d9a6e076903845cd09      1
    293 0xf59baccbacfa9c87970a6ccd52af8dfdf87dff1c      1
    294 0xf5d3b528bd3510a6f97c79557445a0b05bba85e4      1
    295 0xf61030d320e71256a43ec22839db345d80ac84b3      1
    296 0xf7733612846b22d575cf1073f92979592159d2cc      1
    297 0xf9ba5a5ec068a347a021efd32977e49a2724feb0      1
    298 0xfa8016af3abb381723d62f352da0c5ad39c8503a      1
    299 0xfaa614c9a446d267a2885d3ec49cd1b5b7add82c      1
    300 0xfade41403a1eee72d805c42f1eaf5f53a54d0e0d      1

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
