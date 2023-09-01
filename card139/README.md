
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:5871        Length:5871        Min.   : 1.000   Length:5871       
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.053                     
                                           3rd Qu.: 1.000                     
                                           Max.   :41.000                     
         name          
     Length:5871       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18033069 # https://etherscan.io/block/18033069
block_hash <- "0x0d35f2e95ebb81cdac589a856486c16b49d44e59304b12dc7ad6c09cea6cb954"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4759 

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

allow_artist_phase1      <- pick(snapshot, contracts=c("SmartThings","Widgets","SmartInterface","AirShapes","CleanShapes","HallOfFame","Xploration","Foundation"),address_remove=address_remove,address_max=1)
allow_gradient_phase1    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
allow_raw                <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles            <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 193 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0221c3cdb3dc4ffddb0564227d205b7f0cbac1cf      1
      2 0x06163155dfe5bf26ae88556ea1658677000a2c79      1
      3 0x062bab9e487189d4c9792f441d08cff6a960abe5      1
      4 0x06498d95bd6faa6269fa007157698dc0cc44a972      1
      5 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      6 0x08461d342c2161997f62e1383ba32877a3bc50a4      1
      7 0x0925d347f811d264879271d2905f54309eaccb93      1
      8 0x09368bf92aa7e92e450056237bc9fe4877614db2      1
      9 0x0e42d8fcf5166d332ce8df3b65c5e20468fb7359      1
     10 0x100fea55da1316ba7c624a7e5e86ee9861e3ffb7      1
     11 0x10844a040d1c3eebb76286e9916f421aa3f27514      1
     12 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     13 0x1341df844780b66af4ccc98ae0f34be87eabe1d5      1
     14 0x1a4051c7bc8a187027abc9d5c01c74bcfdb64407      1
     15 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     16 0x1b8748ffba680ad31f578033948d240cc75a6bfb      1
     17 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     18 0x1ce8189b2c8a5a4d0e7263fdd60bbe0d21954f46      1
     19 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     20 0x1e1f2a05747be3a55e89ae0c90aa977bcb8a8676      1
     21 0x1e7afff5b18b59e63608dfca6f0da54e32c7d2ed      1
     22 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
     23 0x273fdc49ad8770ac4cbe8476feb86017535528ca      1
     24 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     25 0x28efaf6cab2b5b848778647c95bf256ce3ba3b8d      1
     26 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     27 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     28 0x2cfad32720d343d572c25e761389b495d9a4536b      1
     29 0x2d1cb5ed13c731604a3f1d48dc206d85fe5e52b1      1
     30 0x2d62b7b4ab2aea16f53f78af3544088300040126      1
     31 0x2dfaa32a750a1adc6f8d9c4de370040798787b9a      1
     32 0x2ebe8d7fbcacf047b0e28eaec27355bd34127f40      1
     33 0x2efd420c2a39e0eea94e8a680cd761618fabafd2      1
     34 0x31f9d5bb36e1bd3d816814055b1a801ee5b854ca      1
     35 0x33e8d10459643faab56f1974291cb603b88a09f8      1
     36 0x34d05abb475c6f65f70ff6427eda8db9ce4d40ad      1
     37 0x34ee160d57c7c14d018b6f751fafd1060b560db6      1
     38 0x38b66c508abf9f10d35caffc91e4754e9afda36d      1
     39 0x3a45890964e828ced8f8040eda76e206e5eb7ae3      1
     40 0x3a730f192cfda073603496231672ee458b7a487d      1
     41 0x3aba7f1a35eed304c53afa44912c3af06b01092e      1
     42 0x3cd9c8b49440610a56392be442970489b92a085d      1
     43 0x3d29d165656135fb106529f5959834e024b5cecf      1
     44 0x3da8c6a28a1b8ad5d084453fb4c33059e4636db3      1
     45 0x41807d5a176b8d6b5428ef593ebd96295ddd803a      1
     46 0x423240bab9457f4ae3a69de5d6ad10f07dca1623      1
     47 0x44e84bb96968d341455796ef1f72b6348d8f5411      1
     48 0x45288feed8687238b947ba115b6119ae1c6cfbde      1
     49 0x453af79c641195da5f6e934ef124b474f8bf97fe      1
     50 0x46028de05f5fb0c415a63d73df9cb71450be8f48      1
     51 0x472b6aca55a84727e6d658581ac1a51da97278d5      1
     52 0x48875e7f7fa8519ba16f72ceb4daaf59a70a7fad      1
     53 0x489b19650688b42b6fd5537ebebc7f3ede964a0e      1
     54 0x48a7f9b2d2efbc639fad50425d8a08995ba4a8c7      1
     55 0x48d2f14fce53d43fcab4ab148d739bbcd4c0fb5b      1
     56 0x4a1dafdd58265ac7311a574f8de0e217d5ec7315      1
     57 0x4ad3a8b5f361689c9398bffe63147517501dede4      1
     58 0x4c82a8c78d95ffb5ecadada60b950c577f2ae565      1
     59 0x4e258cc6180e73a69bb0ce18621c8901aed3b792      1
     60 0x4eb8a34d631cbf724a48d5701b57bad24c1e4779      1
     61 0x4ef33f32c39796ae02c3ec21656ace134bab73e9      1
     62 0x4f7ab1bbd95f5d30a90eb6d92dd81d044fc03854      1
     63 0x4fd4c35020cfd14f167bc1d888eb903d7a650768      1
     64 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
     65 0x5000f2b7f12f28b55d27bc8ca8346adc8dc2fc2c      1
     66 0x50836822af94bb883d52ee180682b6035b2603d4      1
     67 0x5124ebdcacab93a3b8fca6c5facabc726760e15e      1
     68 0x5470c5a6fce7447afd2c9be3a0f25e362c093661      1
     69 0x566e65695ef091c343fe0ad1bf541037bf60910e      1
     70 0x57aa36d30d315791a8222d5fbfd76c1608e961cc      1
     71 0x5883fd07e422e2edfdb48cd25578a531534dbd2e      1
     72 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
     73 0x5cec760ba4ba8b2737e18044ec997b9c95613d18      1
     74 0x5dc4ed09e4d8be10315c9a9c46a2d5de7459552c      1
     75 0x5e624a7ad13b5c01d547b1a95a386d1f6147bf56      1
     76 0x602faee794e16604fbb17511b1ad179a728ce61b      1
     77 0x623959f967b770bdb63c5b8beacaf51070466b10      1
     78 0x63f20d7d378fe34fdd03ca2c09040ca96e36e10b      1
     79 0x64752f158b5b57c4d996454873ad364de7511888      1
     80 0x668248df4595e09aa253b31478312748078f7a20      1
     81 0x67ed35ed9d3bbb0b82596f4e0e07499a87aac518      1
     82 0x6ac9f71e6397c98692f0717c9d86884bf9fcd8e8      1
     83 0x6bb4b226b2aecb9848711cba3e82b0d1b3884d95      1
     84 0x6c27fc686ffe133d6c6548cbd849693fb7932a6c      1
     85 0x6ea87e3278f7fd4bc0383a87bc5e6a890712aa57      1
     86 0x7073d3e547921bb3a726f33066582e59a4b95b5f      1
     87 0x74f20a452cc34a7b6c34cb6b9eff11d87b71a450      1
     88 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     89 0x77863da34ca9d4872b86fb9a4973792ca6a11e16      1
     90 0x77ef045d6f08539616a107442b633ede7cf1286d      1
     91 0x78ecde4db3a9fb80339bfd9e59e30d8990b28fef      1
     92 0x7b706fa58a0728587a4e442a270e519e54caad2a      1
     93 0x7c16e57af7faa2929388d2b0f05c8388f844cfde      1
     94 0x7d422cf705af9ea93919b1a6e843b2f9f387562d      1
     95 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     96 0x800cbe0d390ce40a073010a3d2a873d123398216      1
     97 0x81845ec7df4fa5776998053457b8c4fb1e60cf84      1
     98 0x81f2b2afdad2bc0fd8cae42bcedb5d065247145b      1
     99 0x822f341395dca69429e6df7ca1910441067f2784      1
    100 0x854408b508c9dbb1d09f95c2e1c38aebc0f96ccf      1
    101 0x86995f4a229b5a6512bf46b0a65df450f37de109      1
    102 0x8a1a6616253cb617d92f5e539b3570a9eb483127      1
    103 0x8a74bd0aba7e8791336e4427cb9a937177ef8bed      1
    104 0x8ac5b1dc1873994f376276ce008f8adfb2dbdc5b      1
    105 0x8bb8d453b59d3ae166898e44326a1672cebb2877      1
    106 0x8d5e9ea833b4599c9f4253758634798579309ce3      1
    107 0x8f75ee1fa3ee5af135c9b9ccfefe9c61e79ca04c      1
    108 0x901453f39221ff9a2fbeaae1b3201c5315b90912      1
    109 0x902536fae48b23669df5fbe17fe4a92f9bd3aa2b      1
    110 0x9128a39fdb22de4ce3594e2e2e8edd7bd9aba987      1
    111 0x91931a3c54a534b1f107c9e418dd766cfdc08d04      1
    112 0x923dee1d701eb1cac2d122824d1272241a642ffc      1
    113 0x93ba538d15642b703aba9adab81d25d589e116d1      1
    114 0x93e536e96efdfc0230370664ed03255e8656654d      1
    115 0x9457bcb4213199714d0969fa54fa921e3a628297      1
    116 0x96730fc8d482b7751173036acb80f11e3146ac29      1
    117 0x9743cb7141f1fefd7f509532c63a931fc06d26a9      1
    118 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
    119 0x9d3b2006278929a971dab329fb7f9a5899f59505      1
    120 0x9d528bfdef21538303a59d5284801299ddf64e37      1
    121 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
    122 0xa0312ca5418a7e7a3e7caaf7cd0a333fb77be0fb      1
    123 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    124 0xa49e244c93663f249e78cdda7f99363f4268c2ae      1
    125 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
    126 0xa65e83994bc7dbdf4d29811c741d5bbf4a2fcb2d      1
    127 0xa7650e468993e41f32e506e9d74be8998937ed6d      1
    128 0xa77d6b4c95a373dc05e792bd105ec92b53b02633      1
    129 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
    130 0xa8555b9b6c9a2097463ae5bb3427e3d8ef33c929      1
    131 0xab1b9521de0f0a30c43817c66c54c06a95548058      1
    132 0xaeace7f1deba36e5fa1ef9d8eed3e666183c1180      1
    133 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
    134 0xb054b847d2b3d193956ee8b7d4beebd940e55d8c      1
    135 0xb08337cc7610f28279a939ce0a25ad6ad64c28f3      1
    136 0xb0ba3a18c703b470c67b5b3043640d2825592665      1
    137 0xb146772eed0155c9efbd8f6900e05d222fa752a8      1
    138 0xb1fdc4e671889e4f60977540056dd93e5155bd86      1
    139 0xb22898a1e815046bc77c3d2afdc449a5f514d638      1
    140 0xb2bcd6c6a771fa060475f792d6995decbb65093f      1
    141 0xb3402be4a9ef5d3bf43bdca9d58d4203d06b7e3c      1
    142 0xb3d97031367e77a928a0c2825ca8f136a6fbe07e      1
    143 0xb5ac414c576bd2f4291b6c51e167db752c2c4e62      1
    144 0xb5f31eb9e70f88c5997a243dfb59b7097bcd84be      1
    145 0xb63c8f6f2d1bd1ec17a063c422b4282d871704e8      1
    146 0xbd7b263f1e9a423fa344f348babba014d84236f5      1
    147 0xbe719c273b4b2b180d282106dae0e8c917489a27      1
    148 0xbfaab055b643d5a1c9bcc61cd9d5f99e7cd0c7ba      1
    149 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    150 0xc23f53db681d9c4e537d1337848ffcd3c57c2773      1
    151 0xc261c472a5fea6f1002da278d55d2d4463f000ef      1
    152 0xc2f71aa2763996e89484a9bfedbfd204c89ba5cf      1
    153 0xc4210bfd73206c3208ae1f0854ee146ed8f1271b      1
    154 0xc47c0dccfbb4d1c20d53569a288738f22e32275b      1
    155 0xc55a4f326351627af9c19982856b563ff926d412      1
    156 0xc8d578c0db20998a1629606d74dbbc35932004e9      1
    157 0xcafe93d2db682d8031569cee78b994637ba0de8a      1
    158 0xcb5a84c0fbd65b8f00e5c9ccdfd0f14813b3e6c2      1
    159 0xcd148897d736801f1db7109821b9e4366ac266d9      1
    160 0xceaaf79ecd1f11db5fb9259d763c6d0288ce6531      1
    161 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    162 0xd200cd375fe03d9eda297b3bac32d0c8d5f28495      1
    163 0xd3e906e94150bd2b32fccf092db3b82a65853ee2      1
    164 0xd456f6d48992ff44be81de44f3787c9b694106b1      1
    165 0xd4fc759d1dd10936438a5d4c5dc711a85f086c8c      1
    166 0xd6b9ce998d6158645a191fcd17dc5788c6cd21bf      1
    167 0xd8fef4bb781ad488b124b86b0bc7701454060160      1
    168 0xd98009d2d013c74d3ffdbdcab3494d0e8f8bbaae      1
    169 0xda07cbe67aca847143e56294f4148fdb75550c64      1
    170 0xdaf943524f9e226ececfaec41dd0a2a1b1e5699c      1
    171 0xdb8a22004abff0520663c65407b87c2ad64b3b67      1
    172 0xe0a749772f7512983759a8a7dee2f5a39d9ad14c      1
    173 0xe1e0da672b12f7d0d025f4b52512ab1678b2c7fd      1
    174 0xe3f1f50b8aac9c25cc1bceb22a0d668a38edc045      1
    175 0xe6fc0acb3649746e8a7824780f4a55fc94aa5e1f      1
    176 0xe774baa76861390774aca8c13a9b07d9b793f249      1
    177 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    178 0xeaeb42b770e662e653f0de9f4c6f1677102517b7      1
    179 0xebb29ecb65f43e1b245bbf211d103d069144f262      1
    180 0xec61ce77d5a5df233f18b6b4d5a25b5f9a18f8ee      1
    181 0xeeb3ea5f72b9d691469c28270fd99834b17c1447      1
    182 0xef9cc1a7ef8df227437ef06d946eed6009269a6c      1
    183 0xf01994fb823604ad2ab5e3a6bb200bb5ed5665b4      1
    184 0xf0f1a04a214efe01ac5e189e6d9f38cf5d98d84e      1
    185 0xf2e68e8c83264759c9b597b74809ccf59e3cb48b      1
    186 0xf3838d51d6807aa3512cc2f85725a17e517e402b      1
    187 0xf3860788d1597cecf938424baabe976fac87dc26      1
    188 0xf544ae820b584a10a52560a0d359eda5176788da      1
    189 0xf613cfd07af6d011fd671f98064214ab5b2942cf      1
    190 0xf6a325cef74fb60a685bdd5a79ded1ffe45996e8      1
    191 0xfc7030fd3e3ca7541fd4f4a71b9d7b7243e83a37      1
    192 0xfdfe62ab38122be697c455f9d1926d4a56e902fb      1
    193 0xfe99460b27bd6d4a3e8adb29bdf038be6684dd77      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     2 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x1566ae673ae80725bcce901b486c336e6acef465      1
     6 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
     7 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     8 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     9 0x1aa1c4fdb3b3de2b005a363281d0419e10d1b173      1
    10 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    13 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    14 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    15 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    16 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    17 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    18 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    19 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    20 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    29 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    30 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    33 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    34 0x69e68074f1aada957edd39c5eae0069973343f30      1
    35 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    36 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    37 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    38 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    39 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    40 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    41 0x82139687faae8a29851902783e02e699de0e0846      1
    42 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    43 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    44 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    45 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    46 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    49 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    50 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    51 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    52 0xb6883c21885f5091f5064f4e29991bd9cd046473      1
    53 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    54 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    55 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    56 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    57 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    58 0xbf814810b44978de273191fd612aa47f7b69d564      1
    59 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    60 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    61 0xc4e8b752c53df925013e03fe4d2273a8ccb6c545      1
    62 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    63 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    64 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    65 0xcfbf3aa72bcc8af0ba064d9e7b83888495a280de      1
    66 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    67 0xd1f6e5592361257aef68b96df42aef064080c5cc      1
    68 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    69 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    70 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    71 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    72 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    73 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    74 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    75 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    76 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    77 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    78 0xfd22004806a6846ea67ad883356be810f0428793      1
    79 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Allow Random Memes Phase 1

``` r
c(allow_memesRandom_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random300Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007fcf5590179336c826a60c80976623c7bbc29      1
      2 0x00e484da1156202e9dd341ad7ea9c908bb919e96      1
      3 0x01072faa900f4ca3b40944ca815f39837bce193b      1
      4 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
      5 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      6 0x036302a75c0f7dafe15f83c8a1faec282a74a03b      1
      7 0x03abc3de5f7c23d9065cc5610e6496d72208357a      1
      8 0x03ed8e6d50bff0b8c49675f0bba94087d5e579ac      1
      9 0x03ee832367e29a5cd001f65093283eabb5382b62      1
     10 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     11 0x04d0c64d8b303586af5cf6bb37db16bd7b78c43d      1
     12 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
     13 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
     14 0x05cda24eeefd1f24f18dac398f5a612674d3ca5e      1
     15 0x05eec49e27704acf5c5ce31ecfcde4e382d78ba0      1
     16 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
     17 0x06fa75bfaa972845e7d78a6d928357f15a341d6e      1
     18 0x085d07d65b41158a1545eecf05316edb5d163b54      1
     19 0x0a49fbe88cfc413181dbe93c9e3b6184b2264071      1
     20 0x0ce390f18af702cca546297845a4a51d102123cf      1
     21 0x0d5668339c3c17aa7f06ba7fe2667536cf15d748      1
     22 0x0dbe146db9c963bdc56d7445e293c7c3119fa2a1      1
     23 0x0ee7d116e0c89aaefb3c3653cdb71e384766ad11      1
     24 0x0f5a2df7fb9da8c8c7371cfb523ffa527518200e      1
     25 0x0f5d5751d4f300c3d0adbcadc05f0af4f8baf788      1
     26 0x107d1198854fbbcb2e1014c3ffab166b2595586a      1
     27 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     28 0x134309c4cf57bfa43ef66bf20bd0eeccdeb2d80c      1
     29 0x13bfd26cf7d981aae5f8d4d88af4b34b4b1d4da4      1
     30 0x1513f6d3e5468a3a7c4f6b26ffd123cf0dbe4893      1
     31 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
     32 0x15a415c7589efaf71345006d33bfa07a9a42c8c8      1
     33 0x15d7972819b3906fc430b1e7bc26c39e4b9e023a      1
     34 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     35 0x1a4b7409e020140443a02ef26858a7e53ac7d652      1
     36 0x1aa760cf97f1389e03a2c8b4ae7aa1d748d1b666      1
     37 0x1ae50668be1f32179bce00eb203121f00907d808      1
     38 0x1d05a2ec18c7d4707ed4cd40e7e76a680e4618e3      1
     39 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     40 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     41 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
     42 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     43 0x208b4a8ef875a5e4645e23f27343f47fd887d9c5      1
     44 0x20aa168e6c793646f60737399c8466dd643d4281      1
     45 0x215fc4d12967ddc2432a7b8936f2849740edb7bd      1
     46 0x21d79c2be4ab3de2802a60da66f86d497d06102b      1
     47 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
     48 0x24d6ff72eccab41724d488a03e8ea721ec3177a3      1
     49 0x25acc72796bdf4579755708fdbc8409622d224f7      1
     50 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
     51 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
     52 0x273396317b20d90dff0b69a8852ea912240828fe      1
     53 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
     54 0x27e037e0461e3d5587de786ea23441d83772353d      1
     55 0x29722d00967e6131dcd1615b4a56bb3fc00c06be      1
     56 0x2bb2ee28a5bba51db2c104b6c36a6907c21d4d4b      1
     57 0x2be9f18c46df11f05b581131af4d339c20c7254e      1
     58 0x2cb5ae51861a4a6e8568b527cafc3891317ac94d      1
     59 0x2e5e62c8cd9ede2874b6a9c87f843389bfd7cb3b      1
     60 0x2e8f6f8662593134dc8311e312885921855332bc      1
     61 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     62 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
     63 0x32dd9f6885332ccf8633a18cb0c2e25e68fe03d1      1
     64 0x342522ae61de25d48c66807a2cecac4681be3d33      1
     65 0x343193017fd217d19fd983e31db701385c8504f8      1
     66 0x34dea787524a930670862a27bc7234a1897e1bf2      1
     67 0x3543493a76ce5ca362dd6eeef000547de52b6875      1
     68 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     69 0x36d89738405a8325b7c560cab2dd1b88565effd3      1
     70 0x373db7e01ebfb92d9095ae6b8f6e3d24ce6f4d4d      1
     71 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
     72 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
     73 0x387a6107fa226e296710707bac98b718f805ede9      1
     74 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     75 0x38b2739bcb869494cc7953c79c97e3bcad7eac04      1
     76 0x38cb81cea002c1a7659762e57b2878a5b93969f6      1
     77 0x3a30fa661820bf261b39f41a63916cad97b20e60      1
     78 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     79 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     80 0x3b0b262b187001522c34edcafc378500133ab230      1
     81 0x3bd5e344ac629c9f232f921bafdeeec312deac9b      1
     82 0x3de4b60cb19faebf58efea6d4cd99fb5295cf95c      1
     83 0x3e5543a342446999ac11609af4beb82339ca41ae      1
     84 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
     85 0x3fadaba5400ef5ea7a23b1dcc87dd1660186a534      1
     86 0x3fc145ea0fa4290ff0304c689f78dc8fc69788f7      1
     87 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     88 0x41e9e61a699a85daf12dadebbb7e6e517bee6b54      1
     89 0x4322aa46ffed67809862da613725728e2fb8eae3      1
     90 0x4470e0f5a0a3969cb0d5aba71ad4e6c759dfc824      1
     91 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     92 0x47836f027d427e78c06f24eabfdd1d2089fdfd5a      1
     93 0x47e054a8fcf3e27a4bed979c33ab65a5883fe437      1
     94 0x4898203e852b3ed44cc3e8d37f702fd0a7bdac9a      1
     95 0x4a15e3d354043fdefbb8fa77a55da211f4058190      1
     96 0x4b2c1ce6a01981dc3ee79825bdc3f3cd7932bf11      1
     97 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
     98 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     99 0x4e8849962c43d9d7540f64b24bf76704926926ea      1
    100 0x4ea7cc46443cbfd0ae19aec35f08dad83477a52d      1
    101 0x4fed7d0eb3bf1bf1ba69320962c81b132ad4474f      1
    102 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    103 0x513e0a472522e4bd8023c032bbf01e7fb1fe0df3      1
    104 0x514afe6575b5d81cecaa86a6bddf28de2f89ba45      1
    105 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    106 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
    107 0x52d232170f00607b555d97b943094b4ba866f0f0      1
    108 0x541237a3e049f2ef1105694b491152f450aba4db      1
    109 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
    110 0x55bae3706b678ee2d5a6d6d2faec8a41854aaf9a      1
    111 0x55d4a9e5cde6fbe40991e7570e01899891f6b0d2      1
    112 0x56517e41ef464db3753ecfdd2dbcdd2f045b573c      1
    113 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    114 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    115 0x592e480e0066a51eb981b532e275d6576e5730fd      1
    116 0x593cadf136c68c720d446461c1bfee600647c6b8      1
    117 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
    118 0x5bff4631326de92282197696366df57198e38dd3      1
    119 0x5c5edb285b7451b2155ec13c5d2eaff2ec6779ca      1
    120 0x5d181a27b5a2f444ddf4b89d65c88214d7413ada      1
    121 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
    122 0x5e222e726636613c40a87412b9ee04ff53c536e5      1
    123 0x6072b510aa765dd2f31975c9aa65dde39fd1a282      1
    124 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    125 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    126 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
    127 0x6430c11231ec6723e5bbe6ab258f184f5af7c836      1
    128 0x6590ceaf9227887c43c6b3eeffab08f326a1aabe      1
    129 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    130 0x69cc363055d8c3d5e211865e805e145ab9208d57      1
    131 0x6a278927f03422419901b77be4d47700b1f3599c      1
    132 0x6a5ad95a3b0d6d4739de4370f51c8670a4d53700      1
    133 0x6a9b682290dfd41f19a6211c0b219050affbfbcc      1
    134 0x6b9bcb0b854d7c7430baa55167d367d28a8dbf1b      1
    135 0x6df000635d86613626c5208d7c9d71b84e091220      1
    136 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
    137 0x7102a591ded392de38cca9ac35c29e0f6ecef137      1
    138 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
    139 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    140 0x72f315c115408afd9240c5be0946c8ebf7261fb1      1
    141 0x72f52de160ece454a2d75a410f85f4978045882d      1
    142 0x730a2f3421b2967ce8e9ae1f839839944626812c      1
    143 0x734740c70db02f5710207f1d12df47c710665206      1
    144 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
    145 0x7656e919f2af9c7d339333cf29a20c8d7f7bad13      1
    146 0x7738e4dd4fc3b6c95b8925f078363924c0f0b428      1
    147 0x78130d139ee6c5c1b99a49ac70271fe696dc2b3b      1
    148 0x782adafbf47a604f146af4a059908e946eae539f      1
    149 0x7862e990963405616afd1e08cd369433a87adb3a      1
    150 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    151 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
    152 0x79d1dd0770fec45768a285da42c2521603c9b91c      1
    153 0x79e561168835c783240a0637320d308897bd0922      1
    154 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    155 0x7ad225751ed67118e9f54f87e08f64e43ffbef0a      1
    156 0x7b640407513bc16167ef3450fd6339803982e976      1
    157 0x7c76cf8894d42ed11b53236da54f05add1d44073      1
    158 0x7c8f072015d4e29c24088fe55e62381406bd71ec      1
    159 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    160 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    161 0x7dcb39fe010a205f16ee3249f04b24d74c4f44f1      1
    162 0x7f9bea812b9b6c3c4b74ec8aae849a5745cc3ffa      1
    163 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
    164 0x80cf63b40741e041515e6ba6a4d327088540c6a6      1
    165 0x8212642c68a030bab8e4e8d43952c7c4c6cf2903      1
    166 0x82abb5df200d2998db4e80639b145d052ca40062      1
    167 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    168 0x848f8b6c67f4197a53ae3869d3200fac175a00af      1
    169 0x852c03de41993cf180bb8bba83e232a99e2fe635      1
    170 0x853c69418605529a68907aaf7789270e3cf69d97      1
    171 0x85914d049aa004d037cae65bad275ed4147f172e      1
    172 0x86125855ea134f1da31015663fd3c19a657a8d16      1
    173 0x86f0a82dfd9745ec23bc8a72c819193e74962eb3      1
    174 0x892eaf46a9c1cb75c3f230420dcbee6320c26932      1
    175 0x8a6adb9e6e8dba6bddae8bdfb17fb4657720c600      1
    176 0x8a9bfdc136d5d1166497882af5d8c9718f27fbed      1
    177 0x8c0a11eb047c1097c821c159b8e0c2c5f37f81bf      1
    178 0x8c1a163f87915654107148434fe68f60847abab6      1
    179 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    180 0x8c4138dcead987bb50929ecfbeb39ef3e28b4c76      1
    181 0x8ccb07293755004942f4451aeba897db44631061      1
    182 0x8ebfe0a1b5989c87f3c34bec8c160cf9e80b2a78      1
    183 0x8f8a1e5113b11926950185d74567dbafb0aece0b      1
    184 0x8fe89aa22200d1306aed5dad7dbad89c8faf8e26      1
    185 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    186 0x93549cf8047b3e305fb10df7110da213e9429f9c      1
    187 0x9617562c6f338ed18e2e8e88cbbffce6003bae0e      1
    188 0x9969db4034a136650cdb07955cdf5635499a4012      1
    189 0x99980dd0184773ea492c23033e1309609a7e75fe      1
    190 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    191 0x99f70f9aa62bd71a7e0719afc2d6c67c6aaaadbc      1
    192 0x9a659894e5d115846767db0e1685744c452e7a6e      1
    193 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    194 0x9e027bc1bae517189285a207c69615d805670f7a      1
    195 0x9e5f73d2bae44a6d6815eee51c292ba235f04f6e      1
    196 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    197 0xa0ff0e41d6847b1fce33a721a766e9e040027e6e      1
    198 0xa17bfb3a816996ac3a1e26dddcc7850663548c16      1
    199 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
    200 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
    201 0xa2b55ffb7ada20a70a1e6e79073f2f4d6623d72c      1
    202 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    203 0xa56b61e5dee77d1668569dadd699f70eab4e193a      1
    204 0xa73769aed346319287410811639ac3bec8464d55      1
    205 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    206 0xa8450d71b98ca4955c63519ef593ba886c5b2b4f      1
    207 0xaad4210b800f14660ef4068029d428936ebd21fd      1
    208 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    209 0xac1a04679039a1718d3820fbc254ce29269af784      1
    210 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    211 0xae220d647426e368ac321efec83a9d696bf84e7a      1
    212 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
    213 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    214 0xb1db41aa2484e3f5f5a510e07003c29fd1b0f115      1
    215 0xb209e4bd577cecb120fcd1797ee399ddd6533ac5      1
    216 0xb258e243a526e79c4b5c8dd5df490e42eb7927b3      1
    217 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    218 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
    219 0xb6328f4b747b807696da1bb2f574edcbc47682ab      1
    220 0xb6fc3c8f4e5233f2ee2ea1ab84a4d10c61f6d215      1
    221 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    222 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    223 0xb8e82b32ea205b0546d0ece1a175ad00e404dfa1      1
    224 0xb948ea974cb95c9c73c51deac5d9e4464e769a44      1
    225 0xb98be1aba8ae2adef0daa18247273fa06021daae      1
    226 0xbaa4aa9933b4548cff0be2693e7af7e14e9ee49b      1
    227 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    228 0xbdc29767d8bc6dd6b08d64d74c8ecf11b3f5ccf4      1
    229 0xbf949494127d3cd72cd3399d4ab38312757f4d12      1
    230 0xc11a862ba1125a1bc6b4ff11b2ead6b8e73ea52f      1
    231 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    232 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
    233 0xc449f005667bef849261b35accf931a4bace48fb      1
    234 0xc5ed0799c911bf8147680756825725eb038451c8      1
    235 0xc6c3752bdab737b8cfa174e6ecfa620ec0cc162e      1
    236 0xc7c6eec4978799e6a53da79b540432820a349d0b      1
    237 0xca8299a5f070faf59e87f6c4df6e7627d565e006      1
    238 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    239 0xce4122fec66c21b0114a8ef6da8bcc44c396cb66      1
    240 0xce56a746edaf84e65aa6a2bbb45b2328f6a99935      1
    241 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    242 0xcf7c7758512aaf43979b64812cb79bd3f60f4413      1
    243 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    244 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
    245 0xd38e1fb60cd50cf9ae747ecf646b4ff1a310ba55      1
    246 0xd413f436a036b9773d7adccaac10242e27b9da74      1
    247 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    248 0xd70676cfa67abc65a8c39f049d62eea30e3080cf      1
    249 0xd76d392d17379c22ca5e49d167bad1dcaf7eba0d      1
    250 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    251 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    252 0xd939fe2e4776ccaddb823a26eacb45c66d8ad5e9      1
    253 0xda75f63b9cb1cf7668bb0ee34fa718bc1cb5bbc1      1
    254 0xdc36237208adb46959e77a0052843ce5446afab4      1
    255 0xdd5f934edbcfbdb25ef5dd9b8a3868d3633de6e4      1
    256 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
    257 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
    258 0xdf1cf9e3debfefd56286ef4024646f49cc540e37      1
    259 0xdf6cb4483e5b749c2c48be479283247037ad0cff      1
    260 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    261 0xe061ea94f07de987a55a49067b0d7ca3feaffbc7      1
    262 0xe0753cfcabb86c2828b79a3ddd4faf6af0db0eb4      1
    263 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    264 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    265 0xe29bfd5761a18b5c3ac4a4271eb7fadd4c7fb942      1
    266 0xe2cacedac44bd674d9c5e816422cbd603db9cc1c      1
    267 0xe3013b1d58b257684ed9862dbeef81d863e2e0f3      1
    268 0xe422e3c0031396d5eae414e6a0f70c5535b6ec6c      1
    269 0xe4fdfd7f2b4e4926759f2b1ad403d45a1aa9106a      1
    270 0xe560646ef7a69400974d85e818bc0e054bde65c1      1
    271 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    272 0xe61c204d9ab083240a7c8522e636298261ff354e      1
    273 0xe69cae620953aef05e9276f35a7d3cb598bf4b20      1
    274 0xe6c26d2c2376e8997057fdc9ec0076249a6851ce      1
    275 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    276 0xe7985c511d1bf0c7668757650b974513628dea7c      1
    277 0xe7bfc67952b0a48f4ce3db309ab1adda322763dc      1
    278 0xe817ed328e561507dc84d022f42b46f92e003002      1
    279 0xe83105d4e5b144e388d7d35c65c44e0da8c8016f      1
    280 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    281 0xe9e88f56f5431d692446ec723c2f9f9cb4eeca42      1
    282 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    283 0xefc2702d50fb43cec83434d205c89124db029566      1
    284 0xf01bcb0090cd0f734688ce77ae067da58e9d8005      1
    285 0xf0c11838870ca444f5dc0ba0271a36bb9d9f81ec      1
    286 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
    287 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    288 0xf33654f85ba6b567f8841c8c954635b27e14c49d      1
    289 0xf4aa005cdd64b2c80e6cbfc30c427f80b5d1f0b4      1
    290 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    291 0xf5653e48e24e04cc1b14131ab31b776ae8718b34      1
    292 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    293 0xf5d1654521931b52a325d275f3cafa2585a20b2a      1
    294 0xf6dbff32fe8308fd30def6c8629dffa5c315245a      1
    295 0xf9b0ca0da3ea91a40deb0f01b3f3a060d69cf6aa      1
    296 0xfa6bc968cf39a88aa67725463698b6a84bca865c      1
    297 0xfa766f029e703d341cc5386b9a4cf4d52be3b6a3      1
    298 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1
    299 0xfd8835df382b69b3cd498aba0d745fbf6a421d13      1
    300 0xfe73f0b55ee4b411d7d1e4d5d5d4f8834064e2b5      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1

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
