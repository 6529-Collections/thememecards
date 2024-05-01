
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:326         Length:326         Min.   : 1.000   Length:326        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.138                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:326        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19744269 # https://etherscan.io/block/19744269
block_hash <- "0x88f1424201eaecaf3dec7860ef4322725466e7469739f0b818d86bcb89b3af9e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4632 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","MagicalBeasts","Essences","MatMillerAirdrops","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("HippocampusEditions","KnownOriginEditions","MatMillerEditions","JourneyingSpiritEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","MagicalBeasts","Essences","MatMillerAirdrops","MakersPlace","HippocampusEditions","KnownOriginEditions","MatMillerEditions","JourneyingSpiritEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 25 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x2c93b00ff220c5b0fcaef85d6ff01d1f1fd990df      1
     2 0x3b2887ae95e50f22e9a1b225fff308503aed48e7      1
     3 0x3f9cb8a4ab2d3c29608e49e18532cafc40c40b9a      1
     4 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     5 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     6 0x49b13cd6e0ededd6eb93b627cc8364d65de37176      1
     7 0x50a545a4303733bc332918c6dd55d7a8f6dbb234      1
     8 0x59d6779eca6c91ed7679e261b54299b5155eadf0      1
     9 0x64551cd056921112f597c9e20ee4560fb120545a      1
    10 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    11 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    12 0x762da606029d3120735aa1eec15464e265db7a3c      1
    13 0x766977e1e61a75914cfebabc7554d558185b22ea      1
    14 0x8d80802e2a5bdfbb4291e847108802677305d1f6      1
    15 0x990aa52bd46010518fb07521dc32dcba847cbb7c      1
    16 0xa26979cacb7da6b0218f288ac13e88c07a658323      1
    17 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    18 0xb2f2a830e81cb79042a470689e99ddeb1f7df5f5      1
    19 0xc1681754fc94b1856db9c473d956daafb0c043f4      1
    20 0xc171a9d4dda66330c41b6dec0a6b1dc640b2b26d      1
    21 0xc1deb3e48461dddc4a4791b11f89d22079d036fe      1
    22 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    23 0xcf20d98033a4d633252c7de21f8cbfacc62c394e      1
    24 0xdb25085597e4c774e2bbb02207fe52d1c7d0c1e4      1
    25 0xef1ab733bf094ae53226b4deb8713a5e1093d577      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 240 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00d98f64903d990fa7a662f3f76a9106c0988abb      1
      2 0x013cc394844aa993d73ca9a08d89dd6d046f3bcc      1
      3 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      4 0x0264a803979bd0b485a6701f1ab17aab3bcd8ff7      1
      5 0x0291eb432cb4a2613a7415018933e3db45bcd769      1
      6 0x03a852bacc4090fa4ae6ea17bfec463f4d53b986      1
      7 0x064ebe90b4d32e8b18b684f60cab14609d2f6e78      1
      8 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
      9 0x075f344771dbbba0260db5640f6150657b2b3c46      1
     10 0x0773fd9894ebc0aac1196c340d8a41a7ab14a7ae      1
     11 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
     12 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     13 0x0f48669b1681d41357eac232f516b77d0c10f0f1      1
     14 0x116a5035a5a01c25b0fb47eb41c1b8a8954972d7      1
     15 0x1225ab9c0404b9b2b5957a602db8080ac703fe76      1
     16 0x128dd17a6f5921ca248f637d9cda631950932b83      1
     17 0x12dc5b1e1e124b2da9fa339e2f0323be1f180f04      1
     18 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     19 0x14c09da2d0e5b28a73eedf292c6da0e91ecd138f      1
     20 0x1742d3ea8b70985807b88f6b910fa4f7c5a05f7e      1
     21 0x17d7dfa154dc0828ade4115b9eb8a0a91c0fbde4      1
     22 0x189cd6f032fd6e90c064d38b58ed106157902b1d      1
     23 0x18e0f9aadde970d74430cc8636a381ccfcd1f559      1
     24 0x193997d4459380b09a060ba087b2f59b1d928119      1
     25 0x19708f58da99310a737cc60ef978a433dc3aab4d      1
     26 0x1c4420dea13bb5e6c2dd3c14fd9e507bedd44ee9      1
     27 0x1ee8ba8a4b5c5d6e7ec691ff0c93eba580324ef2      1
     28 0x1f7ff9bbf9d212e5e24d3a506e08cba97adb8cc8      1
     29 0x1fa83251e3773011b4ede627b1f16824c8fb8f2e      1
     30 0x21dcbec23ac1152be6c3fa8903c73bcdada378a8      1
     31 0x22cbfccf2f80d8016d66a3189842cfec8e6127bc      1
     32 0x2383eb9a26e7d5aa70eb6c31e9c8c6fc5664061b      1
     33 0x24a0435dd391a6962d8d99ce19f51f2b540412b5      1
     34 0x25840064b55ac9b1531875a6bfc8417feb566ca6      1
     35 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     36 0x269fd4e0c6fbad7c682d8924570dbddda3b092dc      1
     37 0x275b016c870b3092d4243f74e667a1753d169fb5      1
     38 0x281b5dce9cc3a3efab49b7e867beb76becbbf635      1
     39 0x2891d9ed54bb3a643faea7545a619ee63b78b230      1
     40 0x29bbb2014ac9c1e73ce88f6be44be8da900f9758      1
     41 0x2d3e2776404cee093599d429257b6b42b14912d6      1
     42 0x2de14db256db2597fe3c8eed46ef5b20ba390823      1
     43 0x2f61efad6b7577663d83d78e1f63acabd97f099a      1
     44 0x2fc1a595847138479664136166af06c223c6d2b1      1
     45 0x313fe2f013b5ee2e2aac09b59462464db56f7680      1
     46 0x323e6bc721bf81a5292a9c91accdb4315c3f1ec6      1
     47 0x33033761bb4305e48eae6d1a8412eb5175373ecc      1
     48 0x3314b669ea4b1168162803f326d205e5d539c89e      1
     49 0x335da83624f8cdbf69204e54e8c7aac2ed806a0a      1
     50 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     51 0x35f5a3f01bb9cac58796b2c44b310fcf97a21e69      1
     52 0x36b454b1542d0ebdef201630498c37cdc9015f80      1
     53 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
     54 0x37efb5fd87c86b95f691064d26f6655df565a59b      1
     55 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     56 0x39beb60bc4c1b8b0ebeedc515c7a56e7dfb3a5a9      1
     57 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
     58 0x3aeffc77d93d98a12b0e8bcee9d9b9f3fcf0c154      1
     59 0x3d585c0ba07ee860b88aec48e2f901c73105fde3      1
     60 0x3db9ae7d5341249e15bd7d02f248a2377520d365      1
     61 0x4026c330aa37f3aa419061d90818618ec12a38b0      1
     62 0x406bbd4b112e77d7091e36c23b92cca918b5419a      1
     63 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
     64 0x41ef42bb9cb016deeb25d7ca99cfa2f464e3863f      1
     65 0x42e28e8ff63beeb665f5c543cb096564f283661c      1
     66 0x43d3026ab05f1fddfb5621b4f5b35908f9b43890      1
     67 0x44022a7eea59beff273e0d033d76884fa6da0822      1
     68 0x45c7e8d02f9f4c519c33df73705e966b56ac0e54      1
     69 0x465d46e45ab8868035829ac53cc2c25a578aad13      1
     70 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
     71 0x46ddfb43370dc8611e834dbdbe43e8164fbd6001      1
     72 0x478e815c117bd0c33bcc6628d4e30d2776b7b090      1
     73 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     74 0x4906b36edcad5b6f7d6b4a0872cf183e0f72fc61      1
     75 0x4ab68d88c0eebe11c0e44d98a1a24279c8c1c66c      1
     76 0x4abb43963ceb40cb44bc0ee79318539986d1c5a9      1
     77 0x4abc0eb1ef58caec6ca8b2ab4a67cad285a03d35      1
     78 0x4c4098b58150ba60967ae97f74d0382d5cd29562      1
     79 0x4d0f2e4cee59d1c733522b73cd7753d4a11380ce      1
     80 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
     81 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     82 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     83 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     84 0x51c92564137510602710f742250101669b1e6f83      1
     85 0x51ce28855ea50b4eac9656230adf738a5813ca04      1
     86 0x5232d18df504d20242219c54e3c901e2e479465f      1
     87 0x5316909738e83c293b2f8cb445a4ec5e9550bece      1
     88 0x53e9c9405762538bde0b5422429c8e87757ab8af      1
     89 0x55e92035d97d95a835e9a3a50c96da7c36b82b7f      1
     90 0x58454e733bf7d5999607c0e777a9a4df00d60d82      1
     91 0x590ef336b0617900bd61201cc34c3cceecd20381      1
     92 0x59d04f01411b879f9416073edb3382854edd4e08      1
     93 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     94 0x5d7c72e374475d5f3e551c61b2ddd9f852bac419      1
     95 0x5e7d0d9cca8018c4a71d7a01c303a30f36a116a7      1
     96 0x5e9ec7ea604562151e2554018e5a36704c8d9019      1
     97 0x5f8e5ec735dce787ed8a49bbb7a57b1451bb6951      1
     98 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     99 0x6031d4e67f4e098f6663a3a6277bedbe529521af      1
    100 0x608273b078b23ec3c7396628b9b4ef81d8923409      1
    101 0x608d68c29f5b1aadad592c6af035891fa09b73d7      1
    102 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
    103 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
    104 0x63f20d7d378fe34fdd03ca2c09040ca96e36e10b      1
    105 0x640ea6c41cd910db4fb1652b4422255fa3fd4707      1
    106 0x6422d607ca13457589a1f2dbf0ec63d5adf87bfb      1
    107 0x65bd3af92157c57fd06ae0b184bcf01449461f79      1
    108 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
    109 0x675a1243ad6cb448c830e5683ee6bd5f6f9f3a8a      1
    110 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    111 0x6ad3c15acf406e4505b72a848e7cd668fd033d6d      1
    112 0x6d877ace8e049bfac0c8a031a3118d2ffa5ed630      1
    113 0x6dd1c9e3ae3273b1456f3862c2bf7bad12347b33      1
    114 0x6dde7372072036eefaf880600dee87d5019ad2d2      1
    115 0x6e24cc85df5fede8b501d6151955113699c0e12e      1
    116 0x6f04833195de49ab7021c76f6c756ffa41cad262      1
    117 0x748640b4fcad2cd41969387f4b10369b6d687436      1
    118 0x76881c1d4e06eb51303fc7a10b7cd47f171c5fca      1
    119 0x7776caea867683fb174b6b4fc47a2b31423b95e0      1
    120 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    121 0x7af1a8b46fe871968ab5bf70f254acddd23c931e      1
    122 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
    123 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
    124 0x7e3cdbbc82edd873f1cb9aa0ed45062fc6a5dbf6      1
    125 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    126 0x83231461951daa3bc76a775e6e78b3401caf3c1a      1
    127 0x85a30ede5e46029457e20d55427ee8673fad7762      1
    128 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
    129 0x87f7432e6d966452cf93ab5acd337ab305f39e75      1
    130 0x8889ebb11295f456541901f50bcb5f382047caac      1
    131 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    132 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
    133 0x8e4222c444479dbba54309a511a0651d0691d7e3      1
    134 0x90fa9b6cde840a219d2e6517122f3e908f794876      1
    135 0x929045b74b5efc56035be1ba3c851c41bb764fc2      1
    136 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    137 0x96a7e4d9796ec600e8b42cd1b0adb71fcf91390b      1
    138 0x9727938aa47822147a4acd534b088634cb23a4a8      1
    139 0x978354196b348efbe5c2b89a46ddf13030ff121b      1
    140 0x97f61990d7b02404febb8ae72103fbc1e5001602      1
    141 0x98b69d0b81fb1966ebe0af76789425706e5afe7b      1
    142 0x991f2ee7bccc5d8cb9bbd60d3d7118ef4a03538a      1
    143 0x99dccfb625e61fa3980bdda32b55284ad2aa9af5      1
    144 0x9b144b726449507eff54faf4607fdb3d28512417      1
    145 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    146 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    147 0xa161af0e1ab3dbda1f8085b489350fb0df64a51e      1
    148 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    149 0xa25dd72698a703dcbb906cce432cba0586e04fd0      1
    150 0xa2cd656f8461d2c186d69ffb8a4a5c10eff0914d      1
    151 0xa3dc5bab4782df8916181f67d259575b864e97d1      1
    152 0xa3de6b0016f56a5d890b72b32457b8f174ec0424      1
    153 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    154 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    155 0xa52126809c92a62ba2f2966b5d8ce695a802da5e      1
    156 0xa5d154d455d0a545d382ef12d99082d6d4e5ecdb      1
    157 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
    158 0xa7b10d745f3ddc8d039a39293a82705cd5915c01      1
    159 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    160 0xa83cf03df105999d5f35f773260f5b8f8a26d417      1
    161 0xa9889001912c553a8b5fafcd578cb17de6b08ec0      1
    162 0xaadf5e8b700a15880a2d7f0eb0129955cd3118bc      1
    163 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    164 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
    165 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    166 0xacc81faf6846d4a60d722103aa4402534b65e5cf      1
    167 0xacfbc87d4ff16bc6d2df6d383c25ffe7efa56da8      1
    168 0xaea9f1f55a938ae0fe8525648a040c0d27bf917d      1
    169 0xaecc7ca7f81772fb057a535e735338b1b418b8a0      1
    170 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    171 0xb21866e83ce017a631b1244e225b19c85adb3ae2      1
    172 0xb509cd236ae93fdc828a29ff4b5e721993629fd0      1
    173 0xb5dee3d78bb9b876a84e0ae6e43852e080001355      1
    174 0xb5fd638924ffd3a937f3e9724c46ae505cf784eb      1
    175 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    176 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    177 0xbf3ce109e67c0c7bfe6a79c468f0337234425693      1
    178 0xc2065e8c845448b6fd7a3ad168918f2ba6e9d45f      1
    179 0xc30ea36667fdaffd9bc43b89af8818deba5a29ad      1
    180 0xc60c4b71b73d2797c70b3002d1a4e73a9807e5b1      1
    181 0xc61f14dd2fedbba6414ed0f2e3036d50f7919379      1
    182 0xc671da0cf67f9c0e50d8a353d33ad515c9458aa1      1
    183 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    184 0xc723f9bb6c7e8077e4f34942377280ebf098f9b5      1
    185 0xc841fabb79c0b39bd0af850dcd5281022445eb51      1
    186 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    187 0xcbf6a2f2b068dcd6e686b0f90dc3c24fd9739e36      1
    188 0xcbfde5f46e13bf0b69538f4107954681fbaca0b2      1
    189 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    190 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    191 0xcdc35a002a25c2be00b3e613301e3e3f0117f5ad      1
    192 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    193 0xcf4c39ecffa5238cfb34e7301817f9bd838b5a97      1
    194 0xcf4e38da012ea1fc7bc98dc9d940dbac79eb2e77      1
    195 0xd405b50b57d8ec6d2ffa9115f19bec7b0d7917c1      1
    196 0xd4fcc5df55383de8ca56125a1ac5ec5f4bf94904      1
    197 0xd501583d907366541e7936ae249230b1caada4bb      1
    198 0xd8703fc6046d63cb2293384ef91ff493803c6aa6      1
    199 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    200 0xd9c6c5d2d625f6370ce0da78da7847d3f436fb03      1
    201 0xda2a640c0412f9e36af68aa57118eb001cc6ccc9      1
    202 0xdc4471ee9dfca619ac5465fde7cf2634253a9dc6      1
    203 0xdd918db3f4682407a59e6a7761b1933f3c71f6d3      1
    204 0xde1da096a49e503906bd83ab8d10b2c5026df1fa      1
    205 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
    206 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    207 0xe0b43c8426972f1b3113c0ee0ea835539ec8d3dc      1
    208 0xe3efd57b602dc396f7bcdcca34b03890bd53bd6d      1
    209 0xe3fcb080f46ad0581b8ce75ba1873d61a26f6c1f      1
    210 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    211 0xe51db2167c42688789099516771d138f2e650880      1
    212 0xe5dfedcaadf4df48f2fcc56663b89ed0ad73da3e      1
    213 0xe83662f74a5961436490cc88417a107cf06a175a      1
    214 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    215 0xe8a519e334edad1e689a96511a0f7d7b6c4967f9      1
    216 0xe90e640b09fdef82c3e33f2ba2d80d4a784ba0f5      1
    217 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    218 0xeb198f72b1f3ff82ae7c6501e677dca1853da5cf      1
    219 0xebc5733788fd2ff26e785c9396e7410810f5c626      1
    220 0xeed77cd2834edf9e73419cbffcec32b8cab305ff      1
    221 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    222 0xf248a71dc0b1bbd4ffb6093207c78a9eecd1c2ca      1
    223 0xf355672368d62c11bab4b6cb1712e30d49d72987      1
    224 0xf4859a0d738bdda102380637799e24ea11c16d43      1
    225 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    226 0xf531c7a28a3492390d4c47dba6775fa76349dcff      1
    227 0xf60b4342cf2a83451cbc17b40c0f9527908f6c0d      1
    228 0xf64fd8b130a304fdc4431407966ae9f5c0f5858e      1
    229 0xf83d854970c6d7fe7a6f6d26872cc121b996887a      1
    230 0xf9c00bb2c0c8a0999b5f394f83f429c99aa9a16f      1
    231 0xfa43b254584c0edae92e57db903e0eac695908e9      1
    232 0xfb093481439164e493edc309bf3140dd7017fbcf      1
    233 0xfb6216aba2132eed7cd9505b160dc47537e674ef      1
    234 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    235 0xfc5446efe679f109f2772e45ea623caa63791d5e      1
    236 0xfc8e527799f407d675a0c7512789cb72b4e4a8bc      1
    237 0xfd067a330af3e78468c36cced4ea8877b3fec7d3      1
    238 0xfec4213278555d692679c918423816fa99bb74a8      1
    239 0xfeef9d78980083f605c9902c0367df6035d47276      1
    240 0xff839b277ac4a40e458caff575dc24e129298cd1      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 265 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00d98f64903d990fa7a662f3f76a9106c0988abb      1
      2 0x013cc394844aa993d73ca9a08d89dd6d046f3bcc      1
      3 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      4 0x0264a803979bd0b485a6701f1ab17aab3bcd8ff7      1
      5 0x0291eb432cb4a2613a7415018933e3db45bcd769      1
      6 0x03a852bacc4090fa4ae6ea17bfec463f4d53b986      1
      7 0x064ebe90b4d32e8b18b684f60cab14609d2f6e78      1
      8 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
      9 0x075f344771dbbba0260db5640f6150657b2b3c46      1
     10 0x0773fd9894ebc0aac1196c340d8a41a7ab14a7ae      1
     11 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
     12 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     13 0x0f48669b1681d41357eac232f516b77d0c10f0f1      1
     14 0x116a5035a5a01c25b0fb47eb41c1b8a8954972d7      1
     15 0x1225ab9c0404b9b2b5957a602db8080ac703fe76      1
     16 0x128dd17a6f5921ca248f637d9cda631950932b83      1
     17 0x12dc5b1e1e124b2da9fa339e2f0323be1f180f04      1
     18 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     19 0x14c09da2d0e5b28a73eedf292c6da0e91ecd138f      1
     20 0x1742d3ea8b70985807b88f6b910fa4f7c5a05f7e      1
     21 0x17d7dfa154dc0828ade4115b9eb8a0a91c0fbde4      1
     22 0x189cd6f032fd6e90c064d38b58ed106157902b1d      1
     23 0x18e0f9aadde970d74430cc8636a381ccfcd1f559      1
     24 0x193997d4459380b09a060ba087b2f59b1d928119      1
     25 0x19708f58da99310a737cc60ef978a433dc3aab4d      1
     26 0x1c4420dea13bb5e6c2dd3c14fd9e507bedd44ee9      1
     27 0x1ee8ba8a4b5c5d6e7ec691ff0c93eba580324ef2      1
     28 0x1f7ff9bbf9d212e5e24d3a506e08cba97adb8cc8      1
     29 0x1fa83251e3773011b4ede627b1f16824c8fb8f2e      1
     30 0x21dcbec23ac1152be6c3fa8903c73bcdada378a8      1
     31 0x22cbfccf2f80d8016d66a3189842cfec8e6127bc      1
     32 0x2383eb9a26e7d5aa70eb6c31e9c8c6fc5664061b      1
     33 0x24a0435dd391a6962d8d99ce19f51f2b540412b5      1
     34 0x25840064b55ac9b1531875a6bfc8417feb566ca6      1
     35 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     36 0x269fd4e0c6fbad7c682d8924570dbddda3b092dc      1
     37 0x275b016c870b3092d4243f74e667a1753d169fb5      1
     38 0x281b5dce9cc3a3efab49b7e867beb76becbbf635      1
     39 0x2891d9ed54bb3a643faea7545a619ee63b78b230      1
     40 0x29bbb2014ac9c1e73ce88f6be44be8da900f9758      1
     41 0x2c93b00ff220c5b0fcaef85d6ff01d1f1fd990df      1
     42 0x2d3e2776404cee093599d429257b6b42b14912d6      1
     43 0x2de14db256db2597fe3c8eed46ef5b20ba390823      1
     44 0x2f61efad6b7577663d83d78e1f63acabd97f099a      1
     45 0x2fc1a595847138479664136166af06c223c6d2b1      1
     46 0x313fe2f013b5ee2e2aac09b59462464db56f7680      1
     47 0x323e6bc721bf81a5292a9c91accdb4315c3f1ec6      1
     48 0x33033761bb4305e48eae6d1a8412eb5175373ecc      1
     49 0x3314b669ea4b1168162803f326d205e5d539c89e      1
     50 0x335da83624f8cdbf69204e54e8c7aac2ed806a0a      1
     51 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     52 0x35f5a3f01bb9cac58796b2c44b310fcf97a21e69      1
     53 0x36b454b1542d0ebdef201630498c37cdc9015f80      1
     54 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
     55 0x37efb5fd87c86b95f691064d26f6655df565a59b      1
     56 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     57 0x39beb60bc4c1b8b0ebeedc515c7a56e7dfb3a5a9      1
     58 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
     59 0x3aeffc77d93d98a12b0e8bcee9d9b9f3fcf0c154      1
     60 0x3b2887ae95e50f22e9a1b225fff308503aed48e7      1
     61 0x3d585c0ba07ee860b88aec48e2f901c73105fde3      1
     62 0x3db9ae7d5341249e15bd7d02f248a2377520d365      1
     63 0x3f9cb8a4ab2d3c29608e49e18532cafc40c40b9a      1
     64 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     65 0x4026c330aa37f3aa419061d90818618ec12a38b0      1
     66 0x406bbd4b112e77d7091e36c23b92cca918b5419a      1
     67 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
     68 0x41ef42bb9cb016deeb25d7ca99cfa2f464e3863f      1
     69 0x42e28e8ff63beeb665f5c543cb096564f283661c      1
     70 0x43d3026ab05f1fddfb5621b4f5b35908f9b43890      1
     71 0x44022a7eea59beff273e0d033d76884fa6da0822      1
     72 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     73 0x45c7e8d02f9f4c519c33df73705e966b56ac0e54      1
     74 0x465d46e45ab8868035829ac53cc2c25a578aad13      1
     75 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
     76 0x46ddfb43370dc8611e834dbdbe43e8164fbd6001      1
     77 0x478e815c117bd0c33bcc6628d4e30d2776b7b090      1
     78 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     79 0x4906b36edcad5b6f7d6b4a0872cf183e0f72fc61      1
     80 0x49b13cd6e0ededd6eb93b627cc8364d65de37176      1
     81 0x4ab68d88c0eebe11c0e44d98a1a24279c8c1c66c      1
     82 0x4abb43963ceb40cb44bc0ee79318539986d1c5a9      1
     83 0x4abc0eb1ef58caec6ca8b2ab4a67cad285a03d35      1
     84 0x4c4098b58150ba60967ae97f74d0382d5cd29562      1
     85 0x4d0f2e4cee59d1c733522b73cd7753d4a11380ce      1
     86 0x4d9beb5a48378acc8619e1cc8ee6a1af53379ae4      1
     87 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     88 0x50a545a4303733bc332918c6dd55d7a8f6dbb234      1
     89 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     90 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     91 0x51c92564137510602710f742250101669b1e6f83      1
     92 0x51ce28855ea50b4eac9656230adf738a5813ca04      1
     93 0x5232d18df504d20242219c54e3c901e2e479465f      1
     94 0x5316909738e83c293b2f8cb445a4ec5e9550bece      1
     95 0x53e9c9405762538bde0b5422429c8e87757ab8af      1
     96 0x55e92035d97d95a835e9a3a50c96da7c36b82b7f      1
     97 0x58454e733bf7d5999607c0e777a9a4df00d60d82      1
     98 0x590ef336b0617900bd61201cc34c3cceecd20381      1
     99 0x59d04f01411b879f9416073edb3382854edd4e08      1
    100 0x59d6779eca6c91ed7679e261b54299b5155eadf0      1
    101 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
    102 0x5d7c72e374475d5f3e551c61b2ddd9f852bac419      1
    103 0x5e7d0d9cca8018c4a71d7a01c303a30f36a116a7      1
    104 0x5e9ec7ea604562151e2554018e5a36704c8d9019      1
    105 0x5f8e5ec735dce787ed8a49bbb7a57b1451bb6951      1
    106 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    107 0x6031d4e67f4e098f6663a3a6277bedbe529521af      1
    108 0x608273b078b23ec3c7396628b9b4ef81d8923409      1
    109 0x608d68c29f5b1aadad592c6af035891fa09b73d7      1
    110 0x62ac7073454f5b8cd65558711161fc9f3436f76f      1
    111 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
    112 0x63f20d7d378fe34fdd03ca2c09040ca96e36e10b      1
    113 0x640ea6c41cd910db4fb1652b4422255fa3fd4707      1
    114 0x6422d607ca13457589a1f2dbf0ec63d5adf87bfb      1
    115 0x64551cd056921112f597c9e20ee4560fb120545a      1
    116 0x65bd3af92157c57fd06ae0b184bcf01449461f79      1
    117 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
    118 0x675a1243ad6cb448c830e5683ee6bd5f6f9f3a8a      1
    119 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    120 0x6ad3c15acf406e4505b72a848e7cd668fd033d6d      1
    121 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    122 0x6d877ace8e049bfac0c8a031a3118d2ffa5ed630      1
    123 0x6dd1c9e3ae3273b1456f3862c2bf7bad12347b33      1
    124 0x6dde7372072036eefaf880600dee87d5019ad2d2      1
    125 0x6e24cc85df5fede8b501d6151955113699c0e12e      1
    126 0x6f04833195de49ab7021c76f6c756ffa41cad262      1
    127 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    128 0x748640b4fcad2cd41969387f4b10369b6d687436      1
    129 0x762da606029d3120735aa1eec15464e265db7a3c      1
    130 0x766977e1e61a75914cfebabc7554d558185b22ea      1
    131 0x76881c1d4e06eb51303fc7a10b7cd47f171c5fca      1
    132 0x7776caea867683fb174b6b4fc47a2b31423b95e0      1
    133 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    134 0x7af1a8b46fe871968ab5bf70f254acddd23c931e      1
    135 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
    136 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
    137 0x7e3cdbbc82edd873f1cb9aa0ed45062fc6a5dbf6      1
    138 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    139 0x83231461951daa3bc76a775e6e78b3401caf3c1a      1
    140 0x85a30ede5e46029457e20d55427ee8673fad7762      1
    141 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
    142 0x87f7432e6d966452cf93ab5acd337ab305f39e75      1
    143 0x8889ebb11295f456541901f50bcb5f382047caac      1
    144 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    145 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
    146 0x8d80802e2a5bdfbb4291e847108802677305d1f6      1
    147 0x8e4222c444479dbba54309a511a0651d0691d7e3      1
    148 0x90fa9b6cde840a219d2e6517122f3e908f794876      1
    149 0x929045b74b5efc56035be1ba3c851c41bb764fc2      1
    150 0x92b1b677728e36734d59067a4ce53e5bf1f3a2ab      1
    151 0x96a7e4d9796ec600e8b42cd1b0adb71fcf91390b      1
    152 0x9727938aa47822147a4acd534b088634cb23a4a8      1
    153 0x978354196b348efbe5c2b89a46ddf13030ff121b      1
    154 0x97f61990d7b02404febb8ae72103fbc1e5001602      1
    155 0x98b69d0b81fb1966ebe0af76789425706e5afe7b      1
    156 0x990aa52bd46010518fb07521dc32dcba847cbb7c      1
    157 0x991f2ee7bccc5d8cb9bbd60d3d7118ef4a03538a      1
    158 0x99dccfb625e61fa3980bdda32b55284ad2aa9af5      1
    159 0x9b144b726449507eff54faf4607fdb3d28512417      1
    160 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    161 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    162 0xa161af0e1ab3dbda1f8085b489350fb0df64a51e      1
    163 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    164 0xa25dd72698a703dcbb906cce432cba0586e04fd0      1
    165 0xa26979cacb7da6b0218f288ac13e88c07a658323      1
    166 0xa2cd656f8461d2c186d69ffb8a4a5c10eff0914d      1
    167 0xa3dc5bab4782df8916181f67d259575b864e97d1      1
    168 0xa3de6b0016f56a5d890b72b32457b8f174ec0424      1
    169 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    170 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    171 0xa52126809c92a62ba2f2966b5d8ce695a802da5e      1
    172 0xa5d154d455d0a545d382ef12d99082d6d4e5ecdb      1
    173 0xa7093e8a8bb5ba4300ed29822198d0367e7e6d66      1
    174 0xa7b10d745f3ddc8d039a39293a82705cd5915c01      1
    175 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    176 0xa83cf03df105999d5f35f773260f5b8f8a26d417      1
    177 0xa9889001912c553a8b5fafcd578cb17de6b08ec0      1
    178 0xaadf5e8b700a15880a2d7f0eb0129955cd3118bc      1
    179 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    180 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
    181 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    182 0xacc81faf6846d4a60d722103aa4402534b65e5cf      1
    183 0xacfbc87d4ff16bc6d2df6d383c25ffe7efa56da8      1
    184 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    185 0xaea9f1f55a938ae0fe8525648a040c0d27bf917d      1
    186 0xaecc7ca7f81772fb057a535e735338b1b418b8a0      1
    187 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    188 0xb21866e83ce017a631b1244e225b19c85adb3ae2      1
    189 0xb2f2a830e81cb79042a470689e99ddeb1f7df5f5      1
    190 0xb509cd236ae93fdc828a29ff4b5e721993629fd0      1
    191 0xb5dee3d78bb9b876a84e0ae6e43852e080001355      1
    192 0xb5fd638924ffd3a937f3e9724c46ae505cf784eb      1
    193 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    194 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    195 0xbf3ce109e67c0c7bfe6a79c468f0337234425693      1
    196 0xc1681754fc94b1856db9c473d956daafb0c043f4      1
    197 0xc171a9d4dda66330c41b6dec0a6b1dc640b2b26d      1
    198 0xc1deb3e48461dddc4a4791b11f89d22079d036fe      1
    199 0xc2065e8c845448b6fd7a3ad168918f2ba6e9d45f      1
    200 0xc30ea36667fdaffd9bc43b89af8818deba5a29ad      1
    201 0xc60c4b71b73d2797c70b3002d1a4e73a9807e5b1      1
    202 0xc61f14dd2fedbba6414ed0f2e3036d50f7919379      1
    203 0xc671da0cf67f9c0e50d8a353d33ad515c9458aa1      1
    204 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    205 0xc723f9bb6c7e8077e4f34942377280ebf098f9b5      1
    206 0xc841fabb79c0b39bd0af850dcd5281022445eb51      1
    207 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    208 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    209 0xcbf6a2f2b068dcd6e686b0f90dc3c24fd9739e36      1
    210 0xcbfde5f46e13bf0b69538f4107954681fbaca0b2      1
    211 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    212 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    213 0xcdc35a002a25c2be00b3e613301e3e3f0117f5ad      1
    214 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    215 0xcf20d98033a4d633252c7de21f8cbfacc62c394e      1
    216 0xcf4c39ecffa5238cfb34e7301817f9bd838b5a97      1
    217 0xcf4e38da012ea1fc7bc98dc9d940dbac79eb2e77      1
    218 0xd405b50b57d8ec6d2ffa9115f19bec7b0d7917c1      1
    219 0xd4fcc5df55383de8ca56125a1ac5ec5f4bf94904      1
    220 0xd501583d907366541e7936ae249230b1caada4bb      1
    221 0xd8703fc6046d63cb2293384ef91ff493803c6aa6      1
    222 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    223 0xd9c6c5d2d625f6370ce0da78da7847d3f436fb03      1
    224 0xda2a640c0412f9e36af68aa57118eb001cc6ccc9      1
    225 0xdb25085597e4c774e2bbb02207fe52d1c7d0c1e4      1
    226 0xdc4471ee9dfca619ac5465fde7cf2634253a9dc6      1
    227 0xdd918db3f4682407a59e6a7761b1933f3c71f6d3      1
    228 0xde1da096a49e503906bd83ab8d10b2c5026df1fa      1
    229 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
    230 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    231 0xe0b43c8426972f1b3113c0ee0ea835539ec8d3dc      1
    232 0xe3efd57b602dc396f7bcdcca34b03890bd53bd6d      1
    233 0xe3fcb080f46ad0581b8ce75ba1873d61a26f6c1f      1
    234 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    235 0xe51db2167c42688789099516771d138f2e650880      1
    236 0xe5dfedcaadf4df48f2fcc56663b89ed0ad73da3e      1
    237 0xe83662f74a5961436490cc88417a107cf06a175a      1
    238 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    239 0xe8a519e334edad1e689a96511a0f7d7b6c4967f9      1
    240 0xe90e640b09fdef82c3e33f2ba2d80d4a784ba0f5      1
    241 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    242 0xeb198f72b1f3ff82ae7c6501e677dca1853da5cf      1
    243 0xebc5733788fd2ff26e785c9396e7410810f5c626      1
    244 0xeed77cd2834edf9e73419cbffcec32b8cab305ff      1
    245 0xef1ab733bf094ae53226b4deb8713a5e1093d577      1
    246 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    247 0xf248a71dc0b1bbd4ffb6093207c78a9eecd1c2ca      1
    248 0xf355672368d62c11bab4b6cb1712e30d49d72987      1
    249 0xf4859a0d738bdda102380637799e24ea11c16d43      1
    250 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    251 0xf531c7a28a3492390d4c47dba6775fa76349dcff      1
    252 0xf60b4342cf2a83451cbc17b40c0f9527908f6c0d      1
    253 0xf64fd8b130a304fdc4431407966ae9f5c0f5858e      1
    254 0xf83d854970c6d7fe7a6f6d26872cc121b996887a      1
    255 0xf9c00bb2c0c8a0999b5f394f83f429c99aa9a16f      1
    256 0xfa43b254584c0edae92e57db903e0eac695908e9      1
    257 0xfb093481439164e493edc309bf3140dd7017fbcf      1
    258 0xfb6216aba2132eed7cd9505b160dc47537e674ef      1
    259 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    260 0xfc5446efe679f109f2772e45ea623caa63791d5e      1
    261 0xfc8e527799f407d675a0c7512789cb72b4e4a8bc      1
    262 0xfd067a330af3e78468c36cced4ea8877b3fec7d3      1
    263 0xfec4213278555d692679c918423816fa99bb74a8      1
    264 0xfeef9d78980083f605c9902c0367df6035d47276      1
    265 0xff839b277ac4a40e458caff575dc24e129298cd1      1

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
