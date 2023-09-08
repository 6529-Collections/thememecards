
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:12073       Length:12073       Min.   :1   Length:12073      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:12073      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18083069 # https://etherscan.io/block/18083069
block_hash <- "0xb8cd7bffa8477b1652e8946dc58ee0624375ac6f890e8089cdf16de69dc6febd"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4891 

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
      1 0x013868846eb88e0d6b4c235547a9a3d526004c10      1
      2 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      3 0x017c4dc18a2d67e82a49c4359016e50fd8e63233      1
      4 0x02943529d1e692e3281c77f37af56972b09e1498      1
      5 0x02fbd51319bee0c0b135e99e0babed20df8414d2      1
      6 0x0303d8bbc845047b9af305f776f747ca1e25086c      1
      7 0x06afb608e917d228e6b517d7f3a7c8f40135fa73      1
      8 0x07670dd0d0d00ed6ec47cd4073c7ad5ddda32405      1
      9 0x092406b68ed70e6e94db458f9d4fe071240409ae      1
     10 0x0a9ce4ae04d04a54af3949d356b6d6d7a6438f02      1
     11 0x0abd672a6f2a3e2907ee3d8ac9744670c04df8e2      1
     12 0x0c8ce76bfb8ee408e30eaba4e1a00172006a9200      1
     13 0x0dd60f3481a94dde5d5796314a9dc720309f4629      1
     14 0x0e68606d89b301c211896a8889afdd84994ab5b9      1
     15 0x0f01735fb8fa1a88100e5a060272f44c2496987f      1
     16 0x0f1025f754b3eb32ab3105127b563084bfa03a6f      1
     17 0x0f13669bc22cd739f16312076068339124551b29      1
     18 0x0f3c600960b143d46c70b0ec1d9da818a7208f9b      1
     19 0x0f6b0e0ce002b09b60610241ef2518974af0b377      1
     20 0x1012270925a4f629b2f6e006a2416d855c6a2444      1
     21 0x10e1ad76a22ac47db5fb1ccaea17c39d6a1e0b82      1
     22 0x118fe7cec5f26a7c50f6c5bf732f0e6951222ef9      1
     23 0x11b4961ea4a60f548ecf694e7e35ee7e9309f0f5      1
     24 0x12aa1776cc7645d4b5089d1422ef33e19edb30ed      1
     25 0x1471966abfe75cf87fdad8b76d657eab8400f0b0      1
     26 0x147a689478cdd24b5452271972e18eef05ef359b      1
     27 0x149d93dfc34bc8092ad9562a6672394d5edb68a4      1
     28 0x14f62d1bd107f1afdfb1ac24d28c4b74799d88c7      1
     29 0x15694ff1ff07e724d8e0282c448c4731205a0f1a      1
     30 0x16bf2b9490348b21bea767ff4a3d3c82e3059f12      1
     31 0x172004734f85b1b6e3954c2f201dc4beaeb80c65      1
     32 0x17f4d9e092d23d81860d8950c18fdf1dcce75232      1
     33 0x198ad6c547d20d70f2f656a4f48e6c7cfb7b4325      1
     34 0x1aa760cf97f1389e03a2c8b4ae7aa1d748d1b666      1
     35 0x1af42e861037f45e1afde49917d545cda5171a72      1
     36 0x1d9300e0434669e07dca54affefa5c8463208cda      1
     37 0x1e2f87542ba49c77d71cd28d9d8e70f0b642288a      1
     38 0x20d73afd75e18e22ca74558ef967d3f8e6d8a927      1
     39 0x21710fecd114b1336b0918951bb571dd9971db3a      1
     40 0x223e42994eb3c7b4953c86973892ccf0bab4374a      1
     41 0x23b01ce2385b2d16ac0dcea2e46edf21d49be738      1
     42 0x243a39267a8a15d145539257d2d5e2ba0ad078e9      1
     43 0x253c3a24ca84c55094573590a195ecb161b89d55      1
     44 0x266b2d881291c5bb36cf386d16e5f8ebb8e19a71      1
     45 0x27d06aaa0bfb51dbb2d415591f5f4993e6394311      1
     46 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     47 0x2916768f1fea936b6c69830b8e1e3bad5e612255      1
     48 0x294af89e9c338f5f0b54bdfa7db81af97f6cd24b      1
     49 0x2ae11ec93845b283579164436505efd1fa6b1ce9      1
     50 0x2b2713062620ed369a767e0384a513c5b720b561      1
     51 0x2c24dc36c462ea3c10d09791e8886aeaf324a9c2      1
     52 0x2cbdb4c6d4b42a584a217aacbfc9d585827b9cd3      1
     53 0x2d887be27dfba166ee4bdecc1d7788d590d90477      1
     54 0x2ebb29e0eb7310e073b87b1d7b13ea5ac02ce614      1
     55 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
     56 0x2f8f6b4a198423d4a6af82bbf150bf523e70b43d      1
     57 0x2fa5b63690d63f9c3d5deeb32cdc5aebfe1cf32b      1
     58 0x31658df07a63634fd15ea2ab8ca6a739cecc0a55      1
     59 0x31e7bb24ac5e3684345514206d1f2fcdbfed557e      1
     60 0x332ab3f0dd19b601e988024bce17c9795a8c88f7      1
     61 0x34f49d19462bca4169d66921312e2562f1502cef      1
     62 0x35860583266f6c6cad540ef07b4d36ec0d925916      1
     63 0x359a9eecf4df8d936b99c6519516a5569bf14797      1
     64 0x393effe40fe7ecd23446551d89443e9008aefbcc      1
     65 0x39dfeed68d0c848f3927cf8d01663ea639ec5b3f      1
     66 0x3b1311363bff7c9d43263a2f1c325753210d3327      1
     67 0x3bcfd0fdf680ff94dcfb9953b5e6058af531246c      1
     68 0x3c098770c32634140f77401c7086cc2e48ca9ee5      1
     69 0x3fe877b86a298326c13d911a99d59f4c9d195957      1
     70 0x41aec789f14e50b2628fc2fbcc29f909d9a97236      1
     71 0x41de1eed3fe18618f0df558ea363c522ec18e194      1
     72 0x42548a143764550be44273dffb098103625fd965      1
     73 0x43bf1f6a7beeb1f4b9c14314319ef1b90b5c0a56      1
     74 0x4435bc1299b1398fd58f129854de26d60ead6ab4      1
     75 0x45c2e294f01e3decb3c63363317585e7ece20a4e      1
     76 0x46a70a670933c633b179c43f52d6b9c8bbbded13      1
     77 0x46ae568842e3f3d2bb7e2a4f6ca8e38692c6ab86      1
     78 0x46b301aebfbafec36b7fc4383068ab589b149564      1
     79 0x473a17e00cebeda446d43b1136e8ee9d122ac249      1
     80 0x4748f2016fd9a1dcf881dafaa5218c78ce8f8738      1
     81 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     82 0x4839f0a75ede31d26613c7cdccba89be774e9856      1
     83 0x4943cc7813da1697265fded90a2bfb749eba0d89      1
     84 0x49671901d05fabffe166b094f67e2acc59399431      1
     85 0x49d88ae128da4d3332ed2e4e45212c2142f24488      1
     86 0x4a6da59dc4aa1a209850f63b637abe6f7bd8947e      1
     87 0x4acff5f9ba072eedfca73dd706a44fb0b8c6c983      1
     88 0x4b6760682191de7e476b801b7ab42d8e8a5b041a      1
     89 0x4b986ef20bb83532911521fb4f6f5605122a0721      1
     90 0x4babf2f14934fcbeeeac5a71df0aaa20f0eabcc6      1
     91 0x4cf79fe8ee1b0b300133751b671209aa77cf5ce5      1
     92 0x4d6758ba3561a39ad29507363edff464303b4695      1
     93 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     94 0x50d12e66195a67c80531f251ceac41e7e5e67340      1
     95 0x51097fdf7b9ba09e578c4421b99b427097b055a1      1
     96 0x5199d6204b788016353a201b8dad4668a71f1a8a      1
     97 0x523931dcc31e1b202ad6061bb76a360bc3d0a8e3      1
     98 0x525a6e095db26175e21e95c1799532acee0a5606      1
     99 0x527ad5b6d8ad7cdf62f353a55bac5492fa2e73b1      1
    100 0x52dcc21f51dafec21a428bab7707af69c37c2878      1
    101 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    102 0x536cb7bfdb6b66ca30c7ed4a307762f194e05fd8      1
    103 0x568b491fb2b4b9440f6d05e7723df0060fdaef1c      1
    104 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
    105 0x577cb688c7285c88a8770aa67042c94f5d867f2a      1
    106 0x57d579a203d8b5d1cfa6b751307c2d68ab2b15e4      1
    107 0x588d9cde619657b06ca4d5cf4852c0f8e02477ee      1
    108 0x59fa71f26e764aa95030b9e58d5301df83214c71      1
    109 0x5ae2b8e7aab54dc0ffd571c30f76b2d8cc9fc1ac      1
    110 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
    111 0x5da7351a4cb03c33e11f51841bc614d985812821      1
    112 0x5f1c528dff93643d004d9e0b20b25ceb2c596833      1
    113 0x5f27384c4f13ffc993661cad0d2f8563f1f7a4f2      1
    114 0x5f30222b87a2772da131c330197118cbee28b1c6      1
    115 0x5fb33d75a1cfcdc922e736c06e01c1505b4643db      1
    116 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
    117 0x6298e18c627e6fad358f3e4633fa0fd54a88d015      1
    118 0x62c572a9f3ef103b0a9a1ab7b70933b6c92e7cd3      1
    119 0x63f67e0fbd197f7441d92a0d63044c5f94f018e2      1
    120 0x649dc0c4b04e687430016e5d43210825d73afd40      1
    121 0x65a1e9c3548e6847967380aeee69403f6b58ac88      1
    122 0x664f45ed5084abcf2f8e1a95e320b06cc700591b      1
    123 0x669ecdde4a56480c48bc5a7f243cd94072bd5f94      1
    124 0x688bc734e0f452dd46c6b36f23959ea25f683177      1
    125 0x6a5ad95a3b0d6d4739de4370f51c8670a4d53700      1
    126 0x6b9eb48af8d9d4ef58e3e37390f5cfdf6525f5dd      1
    127 0x6dcb2a373398b17ef9b052d547a3785a9ac6985e      1
    128 0x6e3258ce4ecbb70e3cb18692d24e82b675d2e2cf      1
    129 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    130 0x6f3c953df2191831ca9f0fc6ef3cfedf36a9c48c      1
    131 0x6f700e6e498bd11e4c65e1adc2035a398990619e      1
    132 0x70210b9928a21102900dede6ec65cbeb2992dd50      1
    133 0x709c13074ed89a5d8efe996b4f1e2315d833f431      1
    134 0x70af781f6851b057adcd963da042aaca1915050b      1
    135 0x70d9b2084d24823789073d3732a327b2c18a7e6f      1
    136 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    137 0x739bff790640f42a1ebff6731907d46effab2a20      1
    138 0x74619cfc7ec552e5051c181f49428ccd154256f7      1
    139 0x76f499ad599ab376471a62768f1f30a764f2e038      1
    140 0x77d24d4d990d439d0c086d93b932c175238e812f      1
    141 0x77d39e34438232cec261af81b95db52eda14fe18      1
    142 0x78130d139ee6c5c1b99a49ac70271fe696dc2b3b      1
    143 0x78f5761b5a541c2e0f7d1921eaa14a0546f41396      1
    144 0x78ff371955e3a7af7e83873c9775cf382b6a0cdc      1
    145 0x7959aafbde7933776897349888589f488406a2fd      1
    146 0x7a3c82c6b4ad6ee11ecf7f395431ed1135d6e46f      1
    147 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    148 0x7d39fa48a433aa853f7d14130d3dcd6a81b96a77      1
    149 0x7efbdd57601b9d5e98fb325f32626ff90aabc110      1
    150 0x7fb98de44a955da8d0a260433e4c42abc4a99d13      1
    151 0x824ae3bfe2771151db8c7b7defb96a9f67695ade      1
    152 0x8358fc2dd62c00c1a9cd0fcb577309a4cead2471      1
    153 0x84428a30234712da09c639847401b7a84ee30a65      1
    154 0x853e1e59c056da9c3bbf4e780ac0acbfe88d999a      1
    155 0x888c1b86000bbbdb2223a0b89e3c82aec9d94297      1
    156 0x88d8fabbb70d3854b7be95a0237d957cbdea24c5      1
    157 0x8ad272ac86c6c88683d9a60eb8ed57e6c304bb0c      1
    158 0x8b26a9ce34c71f10cff392bf62f3112743ca1740      1
    159 0x8c962009eb45fb6abc9f57a40a2c71098b01b6b8      1
    160 0x8ca0a67d3a84102328ef670d0dea5a8f907d5cb2      1
    161 0x8d32eac23c716357ec7a03fb3f27861e45dc6d9a      1
    162 0x8fcdff1c8a5f19d56e9959ed72419aed8f7e2e44      1
    163 0x918dfe9145e0c5a8ef1c9dcb6e5c0b17f669affb      1
    164 0x9273eb21bc9854d0e06432b5b8c7634ba824ae49      1
    165 0x92e775c5c7cee8e2354002ee1f8efe612c338636      1
    166 0x931e8194d361571cc8476b15f8ce2f6e72d593f5      1
    167 0x9431868e6b2bc5d43d51677dbc5e3a48ef47d6ed      1
    168 0x951e1d85810c7b25dbc0ecceecb4248b4e202412      1
    169 0x952c6b14405566fa50434d23af7075b9e0833676      1
    170 0x954a5549283c936575249d50fae039598b403a25      1
    171 0x966fb9eac572d34e6612ec40ac1a1d86007fa631      1
    172 0x96dfacbb029e1fd05046688965ce581ae71748f8      1
    173 0x9760fed718c26df146e0e47e259609281e0d4954      1
    174 0x97f2966a0ac90d24e80416a3cbaede5b66aa4b00      1
    175 0x9825fea6d8f03447e58844643024281846dfb2e6      1
    176 0x988070d1060fd2c826ccbc9b1446296a96fa75ad      1
    177 0x98dc82470cd96449eff7a21837644e8a0b83f40e      1
    178 0x992077b523edf4ee9850224a7fb9f4499d073e1a      1
    179 0x99d3aa3010b1d90d0c2f4c12a07ab6755b07bba5      1
    180 0x9bbd708653f4f105d5eadd19607b7f360fa787af      1
    181 0x9d8945e2c08751ce18bab505a57073c8de16cd11      1
    182 0x9ed96691c75bfd0f738838ad3516071b33bec557      1
    183 0x9f4c8e21a79ba14de15300c48d0f42e87e45ff08      1
    184 0x9fc7962ad74f3b42d78f3f46b4f82b025b91d059      1
    185 0xa0664d876a0708b015acdfa8df46d1f47e068fcd      1
    186 0xa10d31b628bf5f9fc8e02e91a7564bb2a1e5fea0      1
    187 0xa1486dae9f01e7da3bd8efbd12af0e1be1c73b60      1
    188 0xa30d095a87068b90063d8b2c114bb553579d39e7      1
    189 0xa4d091b1db12e733e1e27f637bb2305672365b85      1
    190 0xa56afee303aa092dacd3364d106aa9b64522c01f      1
    191 0xa58714c5884f5e217df9583d1f8151826e938b02      1
    192 0xa80644a8bbbe1b40d8c54aec3dc2b148574deeda      1
    193 0xa87d967ddb0eacd4ce9e1ae7e4fdb8f95f884d81      1
    194 0xa9e3eddf2616b3d2b142f439575a7811e54223e9      1
    195 0xaa47a4ffc979363232c99b99fada0f2734b0aeee      1
    196 0xaac709a19274b53a9ce099e66c672422c019df7d      1
    197 0xab8d0a6784ce6b16da622c9a3a63f611c5f24cf7      1
    198 0xabb39d6cb2ca91197a979c116a487dd03e0fdbae      1
    199 0xabb77d443c8f50c3f16cf8ae126941ba9153d9fd      1
    200 0xadb723fa57987c83823a5d5df5ea2bf2f1b9c6ea      1
    201 0xae235cca56d4c715303ce00fd3ba124deb28ffa7      1
    202 0xaedeafe1ecf674c40763f883be317ae6cfa0dfff      1
    203 0xaefd26382d84a5f40403a8787b51290908ac9cc0      1
    204 0xb022aad48cc818e4eeac1da96820e8c092785152      1
    205 0xb0807e628889575bf368ca4cde070a903a4b9ea2      1
    206 0xb128b2b054a0d57a0dc3e6cefbc65573cbc29f74      1
    207 0xb1308c5228864109cb519796a7ae4dbea9969592      1
    208 0xb1c1997d94039adb2b62e3bdbdb77edae7365ec0      1
    209 0xb29479ef88dfff8dac466cf6c716e7985c4241c5      1
    210 0xb407763f587f5f8726dcc9eeb7a72c402117016a      1
    211 0xb5263dba4711cad665530424c09f2077c32cebbf      1
    212 0xb5a4e4cb6aaa9f6ea1e00444251682e0f20f86ea      1
    213 0xb63ff8a10c4e456fb40bf88ca513ea397485c49e      1
    214 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    215 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    216 0xb7d032cc77919e17078a172ba98af27925a4a03d      1
    217 0xb8a69fd9b077b1588cc10d807efc4618df22b99c      1
    218 0xb8b6910ed0cf70f92c9a6327838dad479302e7ad      1
    219 0xba64801e510f7f334ef947c668a830cffe95916a      1
    220 0xbad3bf0e52b5d3f91bdcacc5cfa94a97cb9aaad3      1
    221 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    222 0xbb668420d8d8fc286f0d08a5e083ef26ec372fe8      1
    223 0xbb996e83b4071fedb06be1c7c949f3c946a8a80c      1
    224 0xbbb78ad0a8ab71da46b5fd8d2f5bdb3d591f7809      1
    225 0xbbd7251832250b37245eaf22818d8e4451855471      1
    226 0xbcc94f4c922736925e0e9c15391657888e85f435      1
    227 0xbee1500a2fcab995558cf0cca86b986b06f7cce4      1
    228 0xbee66ea0eb74cf2a3cf30f2d955b4cc047d2c78d      1
    229 0xc05869d4aff23777690f2ffeb6794cb879355630      1
    230 0xc113eaca52c51372106f9fa57d6ef19638d61fc9      1
    231 0xc35152b4df68fbda0f5ba3603c7ebc113ac2ba76      1
    232 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    233 0xc8c90c83fd08d7e66703982de7a6177732240ca0      1
    234 0xc99e617b75e9bd50f2b3cec824c234e3f333d8b5      1
    235 0xca6528e0089d0b3090e1705b7276872f39921398      1
    236 0xca9bb916074c6a51281a45cb9b4123a41f79cbac      1
    237 0xcae82145072f10ee8decddb6689987b396dbd879      1
    238 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    239 0xcca3fe904b6a635fa3c4872e3ab2e60acf8b3c00      1
    240 0xcfaedb4d9a764fefca8126d2bcb2a7f233aadf9d      1
    241 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
    242 0xd00911c7f9e5993ea0cd28cb615c6b21a0101666      1
    243 0xd227e6a3e906a4ebb8995745715b6ed5ead1d975      1
    244 0xd3143da6441611570a85a855d842b1d4f55bc28b      1
    245 0xd35c49a60f9d5fec3ea2c43616ca0e2dc84a7af0      1
    246 0xd4af6c2027eec82b6ea16700a1ce0c532873aa82      1
    247 0xd60b8d278a98e068eaf3508e95cf1b0089961288      1
    248 0xd622e4cb95548704b82b2b0e600d154a5c308666      1
    249 0xd646a3cecfd4713095e0951a6dc939886576d9ba      1
    250 0xd66269281f76070f141f8446afb6610ac9feb401      1
    251 0xd66bf38e7c704f0cd7910773f5c8cc3098b6828d      1
    252 0xd722755aa3d8b2e9521209ee5014d0e446b82ff5      1
    253 0xdcaa90d9f3b75cda80764326f6594b58d0585d21      1
    254 0xddeb29cb7c870ae53e6258c1322dc14b904e9051      1
    255 0xde175019eb8e9ceaad16825f63e4849149cbf957      1
    256 0xde35d5d718f5efa776494cc4a50e94cdd6ee2890      1
    257 0xdea12d981110af0e9c8436b50b3144225db65326      1
    258 0xdf12ff846c99ad0beaaa0a6fb7df118a66355293      1
    259 0xdffd933cead5eac96397a0c290c5dd715366bad5      1
    260 0xe093f019e45218949bf210696869665eee186fc3      1
    261 0xe0b4bfaea4ce41d50449e7e3f8d3a53ffe8005c2      1
    262 0xe18e6002c7ce832b2a6a23c6c00c04cff461a56d      1
    263 0xe1f0da1b238db9cb3e51bca447f1210c7134277d      1
    264 0xe3916f1dd2b564f1fcbd0d28b86992120a896a58      1
    265 0xe3ba95950e0e679bd4868e6c60232a742e9fc9b9      1
    266 0xe3f6a0ff75fe1d0e0ab66dea3d6143becf6fb670      1
    267 0xe530507083ecb8d2b474b96544216e0dc92b1883      1
    268 0xe552c715b14c5235d8c2d2277fc05c3c4f3217b0      1
    269 0xe5e18221bf7c0b778db495696a80da77e8417fd1      1
    270 0xe6d2a9b56701d3180d218717141670ad720e479f      1
    271 0xe77563da10227d35e077bd0439a8282dd1c4db32      1
    272 0xe7c7652969ab78b74c7921041416a82632ea7b2d      1
    273 0xe91d95ba259c6d8001a06deaf02f811baa3e2dfa      1
    274 0xe957b0829c6ae6ac461d7365b7927e1ff604114b      1
    275 0xe9d2d1e909e962873260cabde18719c67ea61052      1
    276 0xea94dace59ede50c55d53cc90c26d8ec4ff0b685      1
    277 0xeb7921cda81164f7e711ac0bec1a9243fd10a815      1
    278 0xed35a41508fc8cd2be39de2ae13aa72702393a38      1
    279 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    280 0xeef0c3ca47ee2b81f921ff30a381552f773a5bd2      1
    281 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    282 0xef646a7688ec76c3368b8df136e54394f6fdde9d      1
    283 0xef6914f95fc782a5397cff1686873da77fdeca8f      1
    284 0xef6f856947f7defcbdd1720f6ec578fc115f8ee8      1
    285 0xf2285af9aac7d12574784476b9bc43f8adac034f      1
    286 0xf251b5d633b3ef46307653bcf04c8209cde7d8be      1
    287 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
    288 0xf2d0112da7cda7ac41db6e1fa52ecf7c53f9655a      1
    289 0xf2dca9d0652c43784522397c11b19694c73074a6      1
    290 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    291 0xf474e475fc675a37075ce0d9d5b4367d54c4d8fe      1
    292 0xf49ec27f278295b94d7c3badaaa954f0af278fe0      1
    293 0xf54fe7f5f0f8d13ce9b685b8ad167b466b637f0d      1
    294 0xf5731ba57f5648562d159e912cbc2e921c8cd5d5      1
    295 0xf71c063492a833c6124236a746da25f6f240e44d      1
    296 0xf73f3f426b14fb4f0f62e14429960f23f42e09ae      1
    297 0xf90a1d5081c7c66d13762fbdb5ff3cd5fde3d844      1
    298 0xf92dcbdf576973c02c8c0543f253657877ee1ba1      1
    299 0xfc0c364d64a9664f284a8d74ee27f88d897251e9      1
    300 0xfd21522bb43498e8cfe4b2064bbccdb129eb233c      1

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
