
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:474         Length:474         Min.   : 1.000   Length:474        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.224                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:474        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21850069 # https://etherscan.io/block/21850069
block_hash <- "0x9ccea5132664531a79562c2d434571d13b430489b5e5aff6cfdb0cc4c3f6f21e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4643 

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
  "0x000000000000000000000000000000000000dead",
  "0xA19193B6Bd97798695097e71EAb6d310F99f1955",
  "0x0000000000000000000000000000000000000000"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("Foundation","FoofeeFusions","TheApothecium","Aoife","TheFoofees","METAPRIDELANDCHARITY","FluxEntities","Deraoifetives","FoofeeFriends"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("AoifesSketchbookEditions","OddtraitsEditions","AoifismsEditions","SpiritlingsPartyEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","FoofeeFusions","TheApothecium","Aoife","TheFoofees","METAPRIDELANDCHARITY","FluxEntities","Deraoifetives","FoofeeFriends","AoifesSketchbookEditions","OddtraitsEditions","AoifismsEditions","SpiritlingsPartyEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 87 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0414d789715a989fa8a6bf0bd76fe78e1d2c83b0      1
     2 0x060fb9f0d0529a80f5fb94ecad98df7df99ecb0a      1
     3 0x07bf01c98bdf30b84cf744bfbd0fe0a3409d1096      1
     4 0x0a7d89518246e1bb97d0e26140af4e65909a1edb      1
     5 0x161f9ae7fe8569d52d8ac044c9517f9f479e2d66      1
     6 0x1b8d42f0451bc9077e58b1681abd874f174f376c      1
     7 0x1dd8538299d48c797450fbb9e13ca9538364fe55      1
     8 0x1f916bbf39ab189a9e3d9e1823a7b1a8e9e5f204      1
     9 0x22ded37fe05a998a49d0cd6562a35b3ee74b9920      1
    10 0x245794b1678bfa02c8a58bfeea067e58e68c6652      1
    11 0x2e7173503081160aba4d68204e9cee72e7a9531e      1
    12 0x3048b2a451e197b0ee849d6b573ddb0223d8d6bc      1
    13 0x3462d4f128e214f09a5483ab2613fbf13cd4e57e      1
    14 0x3d6f6043ffc09ad396535cdfacb6e4bc47668e02      1
    15 0x3e43f5b850b3708d6998fef4ddeb9bcf91563f73      1
    16 0x3f1f18c75dbe7919e054993cea62aeea89996dbd      1
    17 0x3f59f26f9b8922d556b3b87d3738e570178b6d01      1
    18 0x4270280f4e8e1519eaf0c0e9d885e3c8556f16c5      1
    19 0x472c2cfa4d5e6e2f4c1ee62c6b4f1a977e3dc943      1
    20 0x48a356748a488a2ad1369d848032706ba9a3e579      1
    21 0x49671901d05fabffe166b094f67e2acc59399431      1
    22 0x49df2026f924bedcb0f6c152651389cccc067ed4      1
    23 0x4bd4dac85258ff43f9e027a85d708095558dd0ac      1
    24 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
    25 0x4e1e4cab1f2bfe4b8725f525611f043b44b2d9e4      1
    26 0x4f85185e32148a42a8a01d76739204508f66e844      1
    27 0x54196238400305778bff5fa200ee1896f6a9d5c2      1
    28 0x549266c062531ec5fd3979b69d10f0720dce3360      1
    29 0x5ccf647b54986ecf5e97acca14cbd8fe1fc2509b      1
    30 0x5e137ee8be2fe8699d8646afb96d6c8412d1130d      1
    31 0x66a4630b2b9e6f8adbc8527a2eff4e0293aef091      1
    32 0x69151d9bfe1a94883ce7084dfc7f730c7f5e6169      1
    33 0x6dfed2b64bb246f7fa8b4c3fd1e6b8085d8c1e06      1
    34 0x70f0f0e6d7c656bfc0bb3cbfe3c82f123c9b7512      1
    35 0x734db537f70823df0996099b4036ab2f2fba4543      1
    36 0x7351415de2c2a9287301d76fdd50cdc1181bc6a0      1
    37 0x73d2a51ba95f1e05fb271b3f4140617c2bd9c691      1
    38 0x799a4f80e42d2a27d86ddf809b5c41181fd02e58      1
    39 0x7d5031135841105b26bcb491c9493babb964ec07      1
    40 0x7ed2c8dee9a3cf927c106a6faf495d2563059e07      1
    41 0x8001c827dfb8dbf7ea4fe31830d7e3dffe15ce95      1
    42 0x804763c163b4ea75c7a388acbb1ccb7d9cd6a6d5      1
    43 0x81d9bfaef5cb2fbc89e1f1bf34502842a8adaa48      1
    44 0x844b3e5535027ba427e3fa9ff1a1b1c3f7268c9d      1
    45 0x865d261a33d2bd1907f91d16982bdb3250f41be0      1
    46 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    47 0x8a1a185e75c9b57b209524c3fc0d449b9247e353      1
    48 0x8d80cbc31145a435f27fb5d366ac1a14557d5b7e      1
    49 0x8f23f6c9f49a7096ec3a9dfdfe828f2bed85bc94      1
    50 0x913de5ecf17fc7027950f0a6dc7c42a72ff02783      1
    51 0x917d6eba229425bb917ecffbd821df6b199d9591      1
    52 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
    53 0x955d95ff27641cbfb1058771a1db82815fb72f91      1
    54 0x9892967a640ee89d8622062bb76046da745d886d      1
    55 0x993a69efe73e3f87df4276e40e81e426385fd2d8      1
    56 0x99581599e8823ee9130380281478e729e926353f      1
    57 0x9a837c9233bb02b44f60bf99bc14bbf6223069b8      1
    58 0x9b4ac495ef5b555d23ce43434c9c21e8cb31add9      1
    59 0x9d8f8b3dbbd2663fa071c01a6f99508d2e0e653a      1
    60 0xa072e41cb9259d82333c5d2a90655a6de45d2b89      1
    61 0xa17bdc0b5fc3c9f48d43449b4b6bf11d03622d01      1
    62 0xa1fa037a1fc2552efeeb0fcc1740f90916dc57b9      1
    63 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    64 0xa46b5474c2c82ee88999a00ce64a5f76fd3e9630      1
    65 0xa8b3f6f8f882ffbfbf0759388b7e25f4d07e6471      1
    66 0xafa14280490a031dfa60bf2368abf9c5b650d31f      1
    67 0xb92eaeee0b31370bb2cb8ecc3aec2bf68404c3c1      1
    68 0xbd6ebd3661ca84298e59b9fd1e0af5a4273be3a9      1
    69 0xc120cd7cf154b135fa8d4391cc3df66865ed22f5      1
    70 0xcc6d3604138c1071a5a5245d40bdbe122eba12ac      1
    71 0xd270414b6f40d50ba0b05e9cdb8bd545d79d7c27      1
    72 0xd4dcdaaa97e3891aa8e9842a328ed739d7af136e      1
    73 0xdeccef22739d711db16eb66fccb6fbc994b1cc45      1
    74 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    75 0xe2e3a4a1d2d7714193378440b9e4804fd15f7640      1
    76 0xe450637a5b515d5bb09d2b75cb4ca89700cc410c      1
    77 0xe5344c65bceaad72615aee7034719d5d5c3e47c4      1
    78 0xe585ffa31f85f0ed29c9e7d1949fa6a3c5fb288a      1
    79 0xe588a2b38f24c72e30535885d645ceba44480d1b      1
    80 0xef60c31cba568cb9e303695b9e6129a4d57e5be6      1
    81 0xeff0338f4fe2ae930c0347f80e6ce4dbcafaa3ef      1
    82 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    83 0xf5cbff0db3a4ae8823fe276cdcee151e3fece685      1
    84 0xf9c00bb2c0c8a0999b5f394f83f429c99aa9a16f      1
    85 0xfc5446efe679f109f2772e45ea623caa63791d5e      1
    86 0xfcc2d631c320035bf59e48a49c5f729ebb452ebc      1
    87 0xfe47cbfb906b65759f34424ca1478be180228c61      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 111 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ec56241b07bf6a63cccfffcc5718aac2f54dc6      1
      2 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
      3 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
      4 0x0a834bbc8d2273c2416ef1cd7a4df735f60cd751      1
      5 0x10ac96adabc52de2909da2b1b3cb6821830449ba      1
      6 0x192f4f2afb058090c24006452b42e4a7e201fcdc      1
      7 0x1aa082360d170156c5281e9dbdf180cf9102b91a      1
      8 0x1c4420dea13bb5e6c2dd3c14fd9e507bedd44ee9      1
      9 0x1e506e6af1dcf5633e105ad36fcfe0ac83dce013      1
     10 0x262456d9a98537f4706324666b28bfa4e9d23446      1
     11 0x2842decf9baeb5ec76988d1261325329848522ae      1
     12 0x2ccff304ef578b238ee82e1d1d53c34e80b48ad6      1
     13 0x2ff7733d65d5d5ee7451e0483dfd76feebb492cf      1
     14 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     15 0x32bc60d5b37ae49178a5a852a9b7b7dd4f54f9d2      1
     16 0x35453038b084f45acd5a3578ea2d392fe6bbb439      1
     17 0x389bbb2d9dd0284b35a24e7a1dfd37e8820b6e21      1
     18 0x39beb60bc4c1b8b0ebeedc515c7a56e7dfb3a5a9      1
     19 0x3aac333b9687703a3a653f8f6dcce890cde4c5fa      1
     20 0x3b6d2ff6f9f04b2717a6ad0c1de4ef4ea2bda25e      1
     21 0x3cf904df518bd0212d9a6289e80aace60728ff91      1
     22 0x3db9ae7d5341249e15bd7d02f248a2377520d365      1
     23 0x3e189905c3f78e7cafcfbea29392c03ec171c8c3      1
     24 0x401ffd21829e8e49e79e6e23fd9683f9e21dd53d      1
     25 0x438a9dde518a5510925289def9815161ff49b00f      1
     26 0x43d4bdeeca8dd891e89ed80dac83b70bf2d1d753      1
     27 0x46ddfb43370dc8611e834dbdbe43e8164fbd6001      1
     28 0x473154b71b4722986018c800f5c3f6146baf8a95      1
     29 0x48a19c6b158491325e84c8837ed8ba95a0403c13      1
     30 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
     31 0x4ce69fd760ad0c07490178f9a47863dc0358cccd      1
     32 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
     33 0x5381315a97df70fc1f47fe8489efb12ad48fc187      1
     34 0x54eef9bcd9e939e1ff4d946e3eabebac1a482e5b      1
     35 0x551cddf9605cfb5d919ef610474d863addc2768a      1
     36 0x57b76626a420772ddfb5ec32379649c4318c3f96      1
     37 0x5b58aeebafe359fe5b19ae3b41048ab859f4bc87      1
     38 0x5df5abd8142993b19d15f216fa0ae4eb516cbddb      1
     39 0x5eb2b0cf7945cb36679f3f11e65709185f739cf0      1
     40 0x5f513bd161c0bb9eff8c651ae868b5946411897e      1
     41 0x60ded73b188885cc33c724bb76ba0724cd4a92f4      1
     42 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
     43 0x63f20d7d378fe34fdd03ca2c09040ca96e36e10b      1
     44 0x643528e02c0f21bf41831d2e9532c3bc2b2f6dac      1
     45 0x668cc52cb5315bc009d011a1ee72a79e09531807      1
     46 0x671d525a7f2bd3aad01343b07c4cebf2d72166c3      1
     47 0x698e9a51378ed9b292e1ea27cd2ba419aee0e4a4      1
     48 0x72e3820d313ce1d0fa07e061c90271af7e4ce594      1
     49 0x733b454dc3cde8ab85dbbb2b8a56344e6c34d7c6      1
     50 0x76881c1d4e06eb51303fc7a10b7cd47f171c5fca      1
     51 0x7b5658e5212377020b953d232875fc59ee6edbef      1
     52 0x7dc7f3932e621a65e5fb7d0cf521c64372db1a92      1
     53 0x7e074d5983c1d9e4a7000bcf22db39b270e08d3a      1
     54 0x810aeb09446c96890ae49cb07fae70c2b2eff2d3      1
     55 0x8282879dba89b5995ab1321d2204744b60657e3e      1
     56 0x84a6eef2246709341f1c36e77da5e18031ee184f      1
     57 0x87ce5ae644c67a180c65d18671065ae82b91ed6d      1
     58 0x8b95f763e9c3ef95ac00ad6fd942512ebaaadbad      1
     59 0x8bfa6b5d35302ac042556c679b0c0bb5faf63dea      1
     60 0x8c0ca6b72104dbb7095d8b349bccc2f60f24bf41      1
     61 0x8c4f916cd87d5876b06b8356960158cbb79515d3      1
     62 0x8dc887f6846a3447474a7c92dcf03b079b1cbbfa      1
     63 0x8f2244aae27cc36d894c86aae6e3136bc1dfeebe      1
     64 0x98532fa1dfea60e03499ea02f747d7e12d801dc0      1
     65 0x9c748a6b2fd26757f6d15b82f4bf7f7aef66f4bb      1
     66 0x9cbbebbbfbe3156c756ff46b87beb6826f2ec630      1
     67 0x9ead888876b3978e8b138d4b6416111255b89e03      1
     68 0xa2c75e8736e446149d5f368db640dd46771b4c2f      1
     69 0xa42caf744d4a6824b7788fa1ed92930d83173921      1
     70 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     71 0xa836d5a27d90c50dc1da972ce712478a5228303a      1
     72 0xacc81faf6846d4a60d722103aa4402534b65e5cf      1
     73 0xad6cdd215aa6fd772188501010c3589b0c090681      1
     74 0xaf153e755f59bb62ba8a5b7e5ffdb71c0ac43305      1
     75 0xb38912b25819659ab8d0cc0ad275953616f6ba42      1
     76 0xb62644da3bb532fda71eb7a3b3fd8977492a723d      1
     77 0xb981602bae363a3f6d6e7f9a911e8df1c1c60efb      1
     78 0xc00a86e1893a90f466d1681198e26887be3de078      1
     79 0xc1c0e9750fab87ac871ea40d005063f3750fe143      1
     80 0xc476d444700b7494d82f0ed8f4a7e985ebb62f53      1
     81 0xc5fdfcb11654b03f88e3b68020ccfe8185d10171      1
     82 0xc625f5c43fcbc45d1805d0c76e1363b78dcb42e2      1
     83 0xcc71dac21cf694648f73a191d675aa5bd3cf5ac7      1
     84 0xcd516ac17657ce94944f2ed084f38bf16d9fed95      1
     85 0xcf42743d1cad547d14beba9b65e70faeefaa12f3      1
     86 0xd122a8abfbcc7fc4865b2799db4fd0837906fd28      1
     87 0xd4543f9f357598e92f45325e6f2f760dc8dbe556      1
     88 0xd58c95d98badd1c8ae832ee0994de54596951011      1
     89 0xd8c88ba578c4c2072656f15831060ea324f7c2b0      1
     90 0xda22f4015a4f6b711f3c82e2c78632cb0f4a286d      1
     91 0xdb270f40269d0714d3b3ec8e3ca79932d8aedda7      1
     92 0xdbed69db1e6e90d55d5b0982904c9290ef7d44fd      1
     93 0xdc11295c27d550c8bd3c5fa8f2e5ad0c32ec0c7b      1
     94 0xde0c12ada427f29871e00587c2fff5b68abb2531      1
     95 0xe0580e24364dd221045c68a674f12666bd3e4253      1
     96 0xe3497b16ee2efd1d954ed88ca4f3c4c97fcf71bd      1
     97 0xe69171feff9b15daa83c0f41edcfba8994ffcd57      1
     98 0xe8ff6b768df816fef4d96cc66dfd0010dca9c32c      1
     99 0xeaaf3cbf36bdc39bf669801e51a011c3a1656da1      1
    100 0xeb5bc3df678d83ed57b0002bf8467e3182828e2f      1
    101 0xedfccb87089f79cad6fcf7321079a6f5424d02a4      1
    102 0xf1ac914cadd6165116bfb5138ee32a90dd0a75d2      1
    103 0xf2c0149f0cff4c19b9819d1084f465df0e1b3795      1
    104 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    105 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    106 0xf60b4342cf2a83451cbc17b40c0f9527908f6c0d      1
    107 0xf63e70d8e4a6534457bc48ab728bf7bedb993100      1
    108 0xf9f3ba83fa1deb56873cd146ffbe3c4b75fb55b6      1
    109 0xfbc1c78ea33496aa0d4e33f430e3def42014195f      1
    110 0xfc6d5a114afd757343495b3be72fd889474eb69e      1
    111 0xfe5946b92078f481dc8e7319f4ccbe8422ea6d21      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 198 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ec56241b07bf6a63cccfffcc5718aac2f54dc6      1
      2 0x0414d789715a989fa8a6bf0bd76fe78e1d2c83b0      1
      3 0x060fb9f0d0529a80f5fb94ecad98df7df99ecb0a      1
      4 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
      5 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
      6 0x07bf01c98bdf30b84cf744bfbd0fe0a3409d1096      1
      7 0x0a7d89518246e1bb97d0e26140af4e65909a1edb      1
      8 0x0a834bbc8d2273c2416ef1cd7a4df735f60cd751      1
      9 0x10ac96adabc52de2909da2b1b3cb6821830449ba      1
     10 0x161f9ae7fe8569d52d8ac044c9517f9f479e2d66      1
     11 0x192f4f2afb058090c24006452b42e4a7e201fcdc      1
     12 0x1aa082360d170156c5281e9dbdf180cf9102b91a      1
     13 0x1b8d42f0451bc9077e58b1681abd874f174f376c      1
     14 0x1c4420dea13bb5e6c2dd3c14fd9e507bedd44ee9      1
     15 0x1dd8538299d48c797450fbb9e13ca9538364fe55      1
     16 0x1e506e6af1dcf5633e105ad36fcfe0ac83dce013      1
     17 0x1f916bbf39ab189a9e3d9e1823a7b1a8e9e5f204      1
     18 0x22ded37fe05a998a49d0cd6562a35b3ee74b9920      1
     19 0x245794b1678bfa02c8a58bfeea067e58e68c6652      1
     20 0x262456d9a98537f4706324666b28bfa4e9d23446      1
     21 0x2842decf9baeb5ec76988d1261325329848522ae      1
     22 0x2ccff304ef578b238ee82e1d1d53c34e80b48ad6      1
     23 0x2e7173503081160aba4d68204e9cee72e7a9531e      1
     24 0x2ff7733d65d5d5ee7451e0483dfd76feebb492cf      1
     25 0x3048b2a451e197b0ee849d6b573ddb0223d8d6bc      1
     26 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     27 0x32bc60d5b37ae49178a5a852a9b7b7dd4f54f9d2      1
     28 0x3462d4f128e214f09a5483ab2613fbf13cd4e57e      1
     29 0x35453038b084f45acd5a3578ea2d392fe6bbb439      1
     30 0x389bbb2d9dd0284b35a24e7a1dfd37e8820b6e21      1
     31 0x39beb60bc4c1b8b0ebeedc515c7a56e7dfb3a5a9      1
     32 0x3aac333b9687703a3a653f8f6dcce890cde4c5fa      1
     33 0x3b6d2ff6f9f04b2717a6ad0c1de4ef4ea2bda25e      1
     34 0x3cf904df518bd0212d9a6289e80aace60728ff91      1
     35 0x3d6f6043ffc09ad396535cdfacb6e4bc47668e02      1
     36 0x3db9ae7d5341249e15bd7d02f248a2377520d365      1
     37 0x3e189905c3f78e7cafcfbea29392c03ec171c8c3      1
     38 0x3e43f5b850b3708d6998fef4ddeb9bcf91563f73      1
     39 0x3f1f18c75dbe7919e054993cea62aeea89996dbd      1
     40 0x3f59f26f9b8922d556b3b87d3738e570178b6d01      1
     41 0x401ffd21829e8e49e79e6e23fd9683f9e21dd53d      1
     42 0x4270280f4e8e1519eaf0c0e9d885e3c8556f16c5      1
     43 0x438a9dde518a5510925289def9815161ff49b00f      1
     44 0x43d4bdeeca8dd891e89ed80dac83b70bf2d1d753      1
     45 0x46ddfb43370dc8611e834dbdbe43e8164fbd6001      1
     46 0x472c2cfa4d5e6e2f4c1ee62c6b4f1a977e3dc943      1
     47 0x473154b71b4722986018c800f5c3f6146baf8a95      1
     48 0x48a19c6b158491325e84c8837ed8ba95a0403c13      1
     49 0x48a356748a488a2ad1369d848032706ba9a3e579      1
     50 0x49671901d05fabffe166b094f67e2acc59399431      1
     51 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
     52 0x49df2026f924bedcb0f6c152651389cccc067ed4      1
     53 0x4bd4dac85258ff43f9e027a85d708095558dd0ac      1
     54 0x4ce69fd760ad0c07490178f9a47863dc0358cccd      1
     55 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
     56 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
     57 0x4e1e4cab1f2bfe4b8725f525611f043b44b2d9e4      1
     58 0x4f85185e32148a42a8a01d76739204508f66e844      1
     59 0x5381315a97df70fc1f47fe8489efb12ad48fc187      1
     60 0x54196238400305778bff5fa200ee1896f6a9d5c2      1
     61 0x549266c062531ec5fd3979b69d10f0720dce3360      1
     62 0x54eef9bcd9e939e1ff4d946e3eabebac1a482e5b      1
     63 0x551cddf9605cfb5d919ef610474d863addc2768a      1
     64 0x57b76626a420772ddfb5ec32379649c4318c3f96      1
     65 0x5b58aeebafe359fe5b19ae3b41048ab859f4bc87      1
     66 0x5ccf647b54986ecf5e97acca14cbd8fe1fc2509b      1
     67 0x5df5abd8142993b19d15f216fa0ae4eb516cbddb      1
     68 0x5e137ee8be2fe8699d8646afb96d6c8412d1130d      1
     69 0x5eb2b0cf7945cb36679f3f11e65709185f739cf0      1
     70 0x5f513bd161c0bb9eff8c651ae868b5946411897e      1
     71 0x60ded73b188885cc33c724bb76ba0724cd4a92f4      1
     72 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
     73 0x63f20d7d378fe34fdd03ca2c09040ca96e36e10b      1
     74 0x643528e02c0f21bf41831d2e9532c3bc2b2f6dac      1
     75 0x668cc52cb5315bc009d011a1ee72a79e09531807      1
     76 0x66a4630b2b9e6f8adbc8527a2eff4e0293aef091      1
     77 0x671d525a7f2bd3aad01343b07c4cebf2d72166c3      1
     78 0x69151d9bfe1a94883ce7084dfc7f730c7f5e6169      1
     79 0x698e9a51378ed9b292e1ea27cd2ba419aee0e4a4      1
     80 0x6dfed2b64bb246f7fa8b4c3fd1e6b8085d8c1e06      1
     81 0x70f0f0e6d7c656bfc0bb3cbfe3c82f123c9b7512      1
     82 0x72e3820d313ce1d0fa07e061c90271af7e4ce594      1
     83 0x733b454dc3cde8ab85dbbb2b8a56344e6c34d7c6      1
     84 0x734db537f70823df0996099b4036ab2f2fba4543      1
     85 0x7351415de2c2a9287301d76fdd50cdc1181bc6a0      1
     86 0x73d2a51ba95f1e05fb271b3f4140617c2bd9c691      1
     87 0x76881c1d4e06eb51303fc7a10b7cd47f171c5fca      1
     88 0x799a4f80e42d2a27d86ddf809b5c41181fd02e58      1
     89 0x7b5658e5212377020b953d232875fc59ee6edbef      1
     90 0x7d5031135841105b26bcb491c9493babb964ec07      1
     91 0x7dc7f3932e621a65e5fb7d0cf521c64372db1a92      1
     92 0x7e074d5983c1d9e4a7000bcf22db39b270e08d3a      1
     93 0x7ed2c8dee9a3cf927c106a6faf495d2563059e07      1
     94 0x8001c827dfb8dbf7ea4fe31830d7e3dffe15ce95      1
     95 0x804763c163b4ea75c7a388acbb1ccb7d9cd6a6d5      1
     96 0x810aeb09446c96890ae49cb07fae70c2b2eff2d3      1
     97 0x81d9bfaef5cb2fbc89e1f1bf34502842a8adaa48      1
     98 0x8282879dba89b5995ab1321d2204744b60657e3e      1
     99 0x844b3e5535027ba427e3fa9ff1a1b1c3f7268c9d      1
    100 0x84a6eef2246709341f1c36e77da5e18031ee184f      1
    101 0x865d261a33d2bd1907f91d16982bdb3250f41be0      1
    102 0x87ce5ae644c67a180c65d18671065ae82b91ed6d      1
    103 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    104 0x8a1a185e75c9b57b209524c3fc0d449b9247e353      1
    105 0x8b95f763e9c3ef95ac00ad6fd942512ebaaadbad      1
    106 0x8bfa6b5d35302ac042556c679b0c0bb5faf63dea      1
    107 0x8c0ca6b72104dbb7095d8b349bccc2f60f24bf41      1
    108 0x8c4f916cd87d5876b06b8356960158cbb79515d3      1
    109 0x8d80cbc31145a435f27fb5d366ac1a14557d5b7e      1
    110 0x8dc887f6846a3447474a7c92dcf03b079b1cbbfa      1
    111 0x8f2244aae27cc36d894c86aae6e3136bc1dfeebe      1
    112 0x8f23f6c9f49a7096ec3a9dfdfe828f2bed85bc94      1
    113 0x913de5ecf17fc7027950f0a6dc7c42a72ff02783      1
    114 0x917d6eba229425bb917ecffbd821df6b199d9591      1
    115 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
    116 0x955d95ff27641cbfb1058771a1db82815fb72f91      1
    117 0x98532fa1dfea60e03499ea02f747d7e12d801dc0      1
    118 0x9892967a640ee89d8622062bb76046da745d886d      1
    119 0x993a69efe73e3f87df4276e40e81e426385fd2d8      1
    120 0x99581599e8823ee9130380281478e729e926353f      1
    121 0x9a837c9233bb02b44f60bf99bc14bbf6223069b8      1
    122 0x9b4ac495ef5b555d23ce43434c9c21e8cb31add9      1
    123 0x9c748a6b2fd26757f6d15b82f4bf7f7aef66f4bb      1
    124 0x9cbbebbbfbe3156c756ff46b87beb6826f2ec630      1
    125 0x9d8f8b3dbbd2663fa071c01a6f99508d2e0e653a      1
    126 0x9ead888876b3978e8b138d4b6416111255b89e03      1
    127 0xa072e41cb9259d82333c5d2a90655a6de45d2b89      1
    128 0xa17bdc0b5fc3c9f48d43449b4b6bf11d03622d01      1
    129 0xa1fa037a1fc2552efeeb0fcc1740f90916dc57b9      1
    130 0xa2c75e8736e446149d5f368db640dd46771b4c2f      1
    131 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    132 0xa42caf744d4a6824b7788fa1ed92930d83173921      1
    133 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
    134 0xa46b5474c2c82ee88999a00ce64a5f76fd3e9630      1
    135 0xa836d5a27d90c50dc1da972ce712478a5228303a      1
    136 0xa8b3f6f8f882ffbfbf0759388b7e25f4d07e6471      1
    137 0xacc81faf6846d4a60d722103aa4402534b65e5cf      1
    138 0xad6cdd215aa6fd772188501010c3589b0c090681      1
    139 0xaf153e755f59bb62ba8a5b7e5ffdb71c0ac43305      1
    140 0xafa14280490a031dfa60bf2368abf9c5b650d31f      1
    141 0xb38912b25819659ab8d0cc0ad275953616f6ba42      1
    142 0xb62644da3bb532fda71eb7a3b3fd8977492a723d      1
    143 0xb92eaeee0b31370bb2cb8ecc3aec2bf68404c3c1      1
    144 0xb981602bae363a3f6d6e7f9a911e8df1c1c60efb      1
    145 0xbd6ebd3661ca84298e59b9fd1e0af5a4273be3a9      1
    146 0xc00a86e1893a90f466d1681198e26887be3de078      1
    147 0xc120cd7cf154b135fa8d4391cc3df66865ed22f5      1
    148 0xc1c0e9750fab87ac871ea40d005063f3750fe143      1
    149 0xc476d444700b7494d82f0ed8f4a7e985ebb62f53      1
    150 0xc5fdfcb11654b03f88e3b68020ccfe8185d10171      1
    151 0xc625f5c43fcbc45d1805d0c76e1363b78dcb42e2      1
    152 0xcc6d3604138c1071a5a5245d40bdbe122eba12ac      1
    153 0xcc71dac21cf694648f73a191d675aa5bd3cf5ac7      1
    154 0xcd516ac17657ce94944f2ed084f38bf16d9fed95      1
    155 0xcf42743d1cad547d14beba9b65e70faeefaa12f3      1
    156 0xd122a8abfbcc7fc4865b2799db4fd0837906fd28      1
    157 0xd270414b6f40d50ba0b05e9cdb8bd545d79d7c27      1
    158 0xd4543f9f357598e92f45325e6f2f760dc8dbe556      1
    159 0xd4dcdaaa97e3891aa8e9842a328ed739d7af136e      1
    160 0xd58c95d98badd1c8ae832ee0994de54596951011      1
    161 0xd8c88ba578c4c2072656f15831060ea324f7c2b0      1
    162 0xda22f4015a4f6b711f3c82e2c78632cb0f4a286d      1
    163 0xdb270f40269d0714d3b3ec8e3ca79932d8aedda7      1
    164 0xdbed69db1e6e90d55d5b0982904c9290ef7d44fd      1
    165 0xdc11295c27d550c8bd3c5fa8f2e5ad0c32ec0c7b      1
    166 0xde0c12ada427f29871e00587c2fff5b68abb2531      1
    167 0xdeccef22739d711db16eb66fccb6fbc994b1cc45      1
    168 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    169 0xe0580e24364dd221045c68a674f12666bd3e4253      1
    170 0xe2e3a4a1d2d7714193378440b9e4804fd15f7640      1
    171 0xe3497b16ee2efd1d954ed88ca4f3c4c97fcf71bd      1
    172 0xe450637a5b515d5bb09d2b75cb4ca89700cc410c      1
    173 0xe5344c65bceaad72615aee7034719d5d5c3e47c4      1
    174 0xe585ffa31f85f0ed29c9e7d1949fa6a3c5fb288a      1
    175 0xe588a2b38f24c72e30535885d645ceba44480d1b      1
    176 0xe69171feff9b15daa83c0f41edcfba8994ffcd57      1
    177 0xe8ff6b768df816fef4d96cc66dfd0010dca9c32c      1
    178 0xeaaf3cbf36bdc39bf669801e51a011c3a1656da1      1
    179 0xeb5bc3df678d83ed57b0002bf8467e3182828e2f      1
    180 0xedfccb87089f79cad6fcf7321079a6f5424d02a4      1
    181 0xef60c31cba568cb9e303695b9e6129a4d57e5be6      1
    182 0xeff0338f4fe2ae930c0347f80e6ce4dbcafaa3ef      1
    183 0xf1ac914cadd6165116bfb5138ee32a90dd0a75d2      1
    184 0xf2c0149f0cff4c19b9819d1084f465df0e1b3795      1
    185 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    186 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    187 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    188 0xf5cbff0db3a4ae8823fe276cdcee151e3fece685      1
    189 0xf60b4342cf2a83451cbc17b40c0f9527908f6c0d      1
    190 0xf63e70d8e4a6534457bc48ab728bf7bedb993100      1
    191 0xf9c00bb2c0c8a0999b5f394f83f429c99aa9a16f      1
    192 0xf9f3ba83fa1deb56873cd146ffbe3c4b75fb55b6      1
    193 0xfbc1c78ea33496aa0d4e33f430e3def42014195f      1
    194 0xfc5446efe679f109f2772e45ea623caa63791d5e      1
    195 0xfc6d5a114afd757343495b3be72fd889474eb69e      1
    196 0xfcc2d631c320035bf59e48a49c5f729ebb452ebc      1
    197 0xfe47cbfb906b65759f34424ca1478be180228c61      1
    198 0xfe5946b92078f481dc8e7319f4ccbe8422ea6d21      1

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
