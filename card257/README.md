
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:507         Length:507         Min.   : 1.000   Length:507        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.026                     
                                           3rd Qu.: 1.000                     
                                           Max.   :14.000                     
         name          
     Length:507        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20344269 # https://etherscan.io/block/20344269
block_hash <- "0x747cfff5be1b3bf609114b70c383f0b0368e5b092c58f4b4b7d4b7abad81ea2b"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4829 

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

allow_artist1    <- pick(snapshot, contracts=c("Olympian","HandsofTime","LEGIONAERIALART","Aqua","Olympian2","Modulo","AerialArt","AERIALINCEPTION","TheCollectibles","Decal"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("RichCaldwellEditions","AerialArtCollectorCardEditions","RCEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Olympian","HandsofTime","LEGIONAERIALART","Aqua","Olympian2","Modulo","AerialArt","AERIALINCEPTION","TheCollectibles","Decal","RichCaldwellEditions","AerialArtCollectorCardEditions","RCEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 208 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00d11913e530c6315dd5d55d0dc49053f401e98b      1
      2 0x00d82eb190946f35fe446dbe082d3be0bb5b80bf      1
      3 0x02a5c980029cb470ac89df2e2de1cf453aee6558      1
      4 0x03409a4010d6d23b3d91aa689a7c9cdc0d947da5      1
      5 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      6 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      7 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      8 0x08f71c03a7169ad9f1c233b19e41c25ee2751560      1
      9 0x09cbf69d6b4f77aea6bf360e17fa187ef1f55d29      1
     10 0x0b2017ae31ffd26a32f513365aa8207882c12e25      1
     11 0x0c71db5adab6631b8511862ab923b27fe331b25e      1
     12 0x0d6de499d0ed3e3875a16e29baa31a2ba334f7d4      1
     13 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     14 0x0dfd32320af10f58cd4a5c2b567b7739ea2d691c      1
     15 0x0f45f177ebb55631b472bd7c3bafda06a653c51b      1
     16 0x139a0975ea36cec4c59447002a875749ac9c460f      1
     17 0x13c942e3f8be4faf966ef03e6038b33d000db22f      1
     18 0x15eaee174343d5c367f65662c603ccf1b901703a      1
     19 0x16150d1ff646c8707b436d7a34adeb8cd4fe916d      1
     20 0x16380b4892e790f9547e92d2dab841189e469b68      1
     21 0x16e23099cca4092c6c7ea3a56506af6dcc58383a      1
     22 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     23 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     24 0x1b052edc4df735f0a5e62c1335c94667e7fdc55e      1
     25 0x1cb65b78c71606090917e425dada70e976f28fc7      1
     26 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     27 0x1da1a6e72d30b24c1ee0c9f192cb1cd382d7edc3      1
     28 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     29 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     30 0x1f575360c38a0947b92407642219ab9d5ab44d9f      1
     31 0x21c1c1d4fbd17eb93282c3119622482d24b71e0c      1
     32 0x230648f035da16d6d9ee3dbdbf3cfca6bd261ebb      1
     33 0x2584fcefec2fe5a4d3e3df5d6f946f7b356acef2      1
     34 0x274c821ddfa5fd5ce2c054f6fe854891e9f49e56      1
     35 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     36 0x2e1bea74e3068404ae07487b95da77c7829641cd      1
     37 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     38 0x3039fb04dd8c5d0d66493df168cd7aba92c57154      1
     39 0x31963b060d71ee24a6d458b75aa85e63b99bd7fb      1
     40 0x3346857e5717918801428184d4e63dfb9f38e51f      1
     41 0x3489164bbc9fe2508ad9ae0c4f0d51556fa40f91      1
     42 0x35946c51a2bb1dcc7d2b417ef23d3e982f121330      1
     43 0x3a88bc828083eadea53f87520250e092b72350f2      1
     44 0x3a94ec43996178f7013988358f58baf18a4cc707      1
     45 0x3adde9c976371a06c95b70030b31fc37117241b4      1
     46 0x3ba9a34e2aed69b23e6e5ede3ac5546dae4abedf      1
     47 0x3c1cf8e31e7b6e46baa9856c8d52f8f10562389f      1
     48 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     49 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     50 0x40b767d119dbc81547cc13c6c55b6ed40a6506f1      1
     51 0x44e9d011df8741be2493e99297e9ad67bb1aa85b      1
     52 0x46ae568842e3f3d2bb7e2a4f6ca8e38692c6ab86      1
     53 0x47279fc734466ee8acfa50aed553ff79770c8779      1
     54 0x47a931aba7dca4c8a61458797ed58f15170efe90      1
     55 0x4835b48fc847e51cd63cca309250c3cd6f4d285e      1
     56 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     57 0x49ad224c1d54596c55d0c223b61f826dad29e7a7      1
     58 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     59 0x4cf01a807af3fbb5fd29a5da34136ba3df0f932a      1
     60 0x4dd5a4d1fcb6f8938aa8ad070aefe1b19e8f9c0c      1
     61 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
     62 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
     63 0x5316909738e83c293b2f8cb445a4ec5e9550bece      1
     64 0x5443415d3807f684095525b9f8f6b892389a40eb      1
     65 0x55b97e94b29cd9bf491fe63011cc23dae6392628      1
     66 0x55e9bf66235fb1461e5d8d7216f1a4bdbe85824f      1
     67 0x563c0e9846812a0678fe028052f8b3c857d85032      1
     68 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
     69 0x57f1c8d52bafcbddcaaa71be11fea6e48e11af11      1
     70 0x5b046272cb9fde317ab73a836546e52b1f2d81f3      1
     71 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     72 0x5f22dbd716ec054a9802d1d0632902398d097c36      1
     73 0x605e2886d8e4632b8acf819af733aa2cbdc94583      1
     74 0x6177adacfde59c3ace55356250b6504488ac513d      1
     75 0x61c5fa0c1bed50eab527e69608d1da11bd4d1bf3      1
     76 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
     77 0x63985b16931a9fe854fe63d3584b24f1ed082f87      1
     78 0x64c0be5f58b4da7253463043359432e032e3edcc      1
     79 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     80 0x66ea2d335291a5afa8d3522eef6adfc3f8746e16      1
     81 0x67ea5cbe1059d53e4580db289721b75b0ca963e3      1
     82 0x692d1fce318f98865203a256b1024e070295c6f4      1
     83 0x69e4767d4c63dbde5ed038eb52da33cf1ab9e4fe      1
     84 0x6a6873c7d9cfa4f79e5651f3676cedae78d4a649      1
     85 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
     86 0x6d745b5bfe909c7ca49499e58db787021537c9cf      1
     87 0x6dd5d07f6868d5db3b055791b9dce502d0f159c8      1
     88 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
     89 0x6f1cc221cd5c49331374908bf9d0f82ed4619423      1
     90 0x6f2148034c9615c0dc0d06589caf73d526d000e4      1
     91 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
     92 0x725a70b1847d1bf3fbb7dcaf43880a9343f308be      1
     93 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
     94 0x7465ce02a9b3eac18b580ebfe87c4ac9fbb2e628      1
     95 0x7616401731473d71e2dece59cb386610d553e546      1
     96 0x768057610a07396e05046af01e7c9b912e4a81f3      1
     97 0x77039399801f462b4ed13444a266b16355c471bf      1
     98 0x773d715200ab5c07f39db9772e3c83c48534a585      1
     99 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
    100 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    101 0x79dc911ba6f8424a4b6158ea0b24168a8e0e0fe4      1
    102 0x7af2e406bcc85580f2994f6cee959b280c6e0c32      1
    103 0x7b50b1f2634fa38d078eb067a4f8f22a666b49a2      1
    104 0x7be90f10f9c6cd02f32401c4929e5a5dbaa0a51b      1
    105 0x7c2f06ab61a94fe9760cc8d9ad35a33c0472cb8e      1
    106 0x7c59a1ddd7b8e5ba683941ea67b20b93864ad8f9      1
    107 0x7dab31a034ef37229a53d270dd973e4f559665b3      1
    108 0x7fdda177cc70b317d90d6259e6d919f9ae60ab48      1
    109 0x8099dcb5d0fbe0c0b45f10c4eba244428a4a725c      1
    110 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    111 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    112 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    113 0x8497277c9339170a0420e86da8352e0c084624cd      1
    114 0x856d040ffd5e86860d8025521fba6633c4006b52      1
    115 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    116 0x87bf447ac29ba9498e5c0859513c39a0931f303a      1
    117 0x8912f371133b8474229e52e71174d3c7dfca6e06      1
    118 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
    119 0x8df65a9e58fa621c404f6353181d9f2cd9a0359d      1
    120 0x8e8f3e4891f87c42b6f512b38be9bc7583198b3e      1
    121 0x8f06975b0c3cc46087134339fb22ee2d46d2106d      1
    122 0x909d3c000baa83a64811ec0f41da55ec0ebb8a67      1
    123 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    124 0x9b717cf7313315f27e8fa93c822f3e9cda07e8b2      1
    125 0x9d2e431a3010d9f3ac3c1f5ace093ac1f1d102b7      1
    126 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
    127 0x9d75d2f3c33f93468689fc2ba0dddbca831d5b8d      1
    128 0xa0bd213eba4a6117d2ad12d86dee6876eb5f034c      1
    129 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    130 0xa14beaa3bcf95d4e13a8aff1b34aa4468edb38ba      1
    131 0xa283dfd91be8d638ea7e8d4800c20980ee2dda68      1
    132 0xa3eac0016f6581ac34768c0d4b99ddcd88071c3c      1
    133 0xa454daff2609603b169e6290fe69667705ed11fb      1
    134 0xa5430730f12f1128bf10dfba38c8e00bc4d90eea      1
    135 0xa5dce742f3775059d9406342d2ecbb0ff8e3a3c2      1
    136 0xa7f66d3ff024e639191c89170463b6ac4ff1478d      1
    137 0xa86cd6bfe453ad3bd3bfbb263d223a1a39eebeab      1
    138 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    139 0xa96cbc4c3651b25aebf9ec28d478781bc366bac7      1
    140 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    141 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    142 0xace1e41547899bda3db142ca28bc6f6de92e362b      1
    143 0xace3898b6e111f8154234e22d248cc3165cee849      1
    144 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    145 0xaf4a749f3e218a67e40fbc295ba58f93c890a27d      1
    146 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    147 0xb1f882290617f9bdb50dbf970e196839f74735cb      1
    148 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    149 0xb79c26fafaafeb2835ff175d025db1b9deeedf5e      1
    150 0xb8ad88b9d89aca9261ddcbfee62177ed86d99454      1
    151 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
    152 0xbd440f1465cb5133b98dc6e8f6514feba6aa8a54      1
    153 0xbfe6ab779517544df703159ea41e7d8a71db6ef0      1
    154 0xc0858cb8bb6ffbada00da01a74193a8ff4bb64b9      1
    155 0xc08c48349322d72d2372aabea4ef6e9153960c3d      1
    156 0xc15add7eb1ba708bc7189cef6a4c47200b77a52b      1
    157 0xc1855da46c382600e1571484d0880d4967e85aee      1
    158 0xc25461af5e224b7aceeaf0eff443a239fd5040e6      1
    159 0xc2de42cf18e7b5d9774050eba2d5e2a33bf3bf63      1
    160 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
    161 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    162 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    163 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    164 0xc6f6c89f7f297b5230d2fd028ac06f0677c1857b      1
    165 0xc7d3e2ad7a2c712b355688b0022580542769d81e      1
    166 0xc8301c772bc161d261acca1de406012afbbcf854      1
    167 0xca970bc37e2804289fcf651123afdf919d6da235      1
    168 0xcb3408bad3192777e561b362e8467c213231ef9f      1
    169 0xcb5d4b21fed74edc68782fe07d084759c9c54151      1
    170 0xccd021ac40448e74641c34e71aaf617afa951ba3      1
    171 0xcce5ad053b8a02c2ecaa8c00412ef9915339542f      1
    172 0xcf0df56e67f469f390e5c458c9debd875b6a5eb3      1
    173 0xd050c97e76f882187c781feb2e74345227e2f710      1
    174 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    175 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    176 0xd578a6d0958537c39cd9d6599ab83ed7f410f93b      1
    177 0xd70676cfa67abc65a8c39f049d62eea30e3080cf      1
    178 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    179 0xd7c45ea3e2eff97007d7c9f96d37141167b3fa7b      1
    180 0xd849843971653d1d3c8d40864ab4f16080fdcbf4      1
    181 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    182 0xdaf9472ecbfab430969ffe4b4fae2ab546198391      1
    183 0xdbc50b04023fd956f13d3f2625df7fbb022b996e      1
    184 0xdc29516f75bb6d8e7e76566062cd90f8c3087072      1
    185 0xdc9f06ec53b831934a1425e769ac498e3fef06cd      1
    186 0xdd0f08dad5cb2a920f6f9a72b2d9f44699df3a0a      1
    187 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    188 0xe1268b49d3e4e559f466239df4bee1d5cae04356      1
    189 0xe72c4bea4b5f5caabeaca6cd38894f8ede1f2e16      1
    190 0xe88605c4d8a7ba1e4a1c897d0e82c6b27311ccef      1
    191 0xe99b70874dd7785e7d88b0a629afe80ac1e19634      1
    192 0xebfd394de050a10b0a9d8408ef395aaa8d269de8      1
    193 0xec876db33e7083d9ddcaf210a92c5b0182f8b2f8      1
    194 0xecba5f51925e6ccec26da38dcd7d5305f6bdfbcb      1
    195 0xef77bc2f8cb4ec186c277089eb164c1aa7a1e14a      1
    196 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    197 0xf14afa6a2960a3e79a9a1d617405907f67a7f9d4      1
    198 0xf1da6e2d387e9da611dac8a7fc587eaa4b010013      1
    199 0xf22742f06e4f6d68a8d0b49b9f270bb56affab38      1
    200 0xf2996f01814cc1fb1d0aa776678668fd0f91b795      1
    201 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    202 0xf4d63913a748a6bbd99fbca027c58f3e9ee72a0c      1
    203 0xf86a588f3604e3b12899f710e3d572f76ffb94b8      1
    204 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    205 0xfba7737aea0969ce95cc7f1879b58e8fb8c16836      1
    206 0xfc0344564a36d3f572beca9cdaf67ca123bd4b45      1
    207 0xfdb26046b3138f5a812fffe027527c0bf9c4d686      1
    208 0xfdd3980797820a22e1eda8460c2dec333a9c3ae2      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 60 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04173063c0c491470b735954790f94ed307aae9d      1
     2 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     3 0x0970b838d4d209b21de42ca52f7e0fe79811de26      1
     4 0x09ab0b019fd9c44316678b5a62ced52d179b0c1a      1
     5 0x0c96e77528ab52422495a5474697e7630504c739      1
     6 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     7 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     8 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
     9 0x22d0567f1d331f6e18452e2bb11f0ed3591ac485      1
    10 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
    11 0x31f5afe73e35de36ec0b99ab683883af4dfc8dce      1
    12 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    13 0x3686a4b272c646ef6fbe34377337d95db7356e63      1
    14 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
    15 0x3da7b8a0353fb926168a2464ffc4160be49b903b      1
    16 0x3deed956b999b83361b85bff31d388c35125411d      1
    17 0x42689518d72a0fdbc34cfe30aa36d89ed8beb5bb      1
    18 0x4639e37f75a4599fe22371d618c9336da47cad1a      1
    19 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
    20 0x5178b38849eb9200ad681309ee4a6d5b00c23e51      1
    21 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    22 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
    23 0x6439e9006c8da5f686eafa4718b295f531b38083      1
    24 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    25 0x6a6d335d1ca6c4539b7cee6476b706685f97bc78      1
    26 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    27 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    28 0x6ebeba28d04a68bbb137380cf22edd38fd993dfb      1
    29 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    30 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    31 0x7d422cf705af9ea93919b1a6e843b2f9f387562d      1
    32 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    33 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    34 0x8030e495ea55d8ec8ae1e10bf7498bf7f0eac1aa      1
    35 0x8b7d031fb0d8aab5a985583c6c56c4fffab27ee7      1
    36 0x8e7a0b4c6550e22880081451c4f75e497e037e3e      1
    37 0x92aebbcb31a62b83bc96a9691da2d6d6f0f1ce85      1
    38 0x92c91e0ae69264c52b630ad522eb9781cda31d82      1
    39 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    40 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
    41 0x9a78c0913396f8607eac7c4ae211a9138c2a1fac      1
    42 0xa43711f5dcc3559a1e07ec124fd167e26c9bad43      1
    43 0xabb77d443c8f50c3f16cf8ae126941ba9153d9fd      1
    44 0xaed0ab8695fbd707424bd07a2e8ae1c6d7c94a8d      1
    45 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    46 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    47 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    48 0xd2628fb21499c690f5015af5df410a6fc72df72f      1
    49 0xd407e5b3f487b4b74c4f3f5c8a10a801285146a2      1
    50 0xd8478053a45bfcdc7a8411b27c7329274c49de05      1
    51 0xda86d12e752631964895e2ae91c8fc8465967d2a      1
    52 0xe6ba7ce1bbd7b811d891c2d4c09d7295bb96752c      1
    53 0xeb68669d321e1459900d83595818ce1313a4d90f      1
    54 0xec63e88ba3d5482cbfa59872d2d889ba57796e17      1
    55 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    56 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    57 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    58 0xf3860788d1597cecf938424baabe976fac87dc26      1
    59 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
    60 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 268 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00d11913e530c6315dd5d55d0dc49053f401e98b      1
      2 0x00d82eb190946f35fe446dbe082d3be0bb5b80bf      1
      3 0x02a5c980029cb470ac89df2e2de1cf453aee6558      1
      4 0x03409a4010d6d23b3d91aa689a7c9cdc0d947da5      1
      5 0x04173063c0c491470b735954790f94ed307aae9d      1
      6 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      7 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      8 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      9 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
     10 0x08f71c03a7169ad9f1c233b19e41c25ee2751560      1
     11 0x0970b838d4d209b21de42ca52f7e0fe79811de26      1
     12 0x09ab0b019fd9c44316678b5a62ced52d179b0c1a      1
     13 0x09cbf69d6b4f77aea6bf360e17fa187ef1f55d29      1
     14 0x0b2017ae31ffd26a32f513365aa8207882c12e25      1
     15 0x0c71db5adab6631b8511862ab923b27fe331b25e      1
     16 0x0c96e77528ab52422495a5474697e7630504c739      1
     17 0x0d6de499d0ed3e3875a16e29baa31a2ba334f7d4      1
     18 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     19 0x0dfd32320af10f58cd4a5c2b567b7739ea2d691c      1
     20 0x0f45f177ebb55631b472bd7c3bafda06a653c51b      1
     21 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     22 0x139a0975ea36cec4c59447002a875749ac9c460f      1
     23 0x13c942e3f8be4faf966ef03e6038b33d000db22f      1
     24 0x15eaee174343d5c367f65662c603ccf1b901703a      1
     25 0x16150d1ff646c8707b436d7a34adeb8cd4fe916d      1
     26 0x16380b4892e790f9547e92d2dab841189e469b68      1
     27 0x16e23099cca4092c6c7ea3a56506af6dcc58383a      1
     28 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     29 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     30 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     31 0x1b052edc4df735f0a5e62c1335c94667e7fdc55e      1
     32 0x1b609fda3c97e90003be00c8223026e0d99f49b4      1
     33 0x1cb65b78c71606090917e425dada70e976f28fc7      1
     34 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     35 0x1da1a6e72d30b24c1ee0c9f192cb1cd382d7edc3      1
     36 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     37 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     38 0x1f575360c38a0947b92407642219ab9d5ab44d9f      1
     39 0x21c1c1d4fbd17eb93282c3119622482d24b71e0c      1
     40 0x22d0567f1d331f6e18452e2bb11f0ed3591ac485      1
     41 0x230648f035da16d6d9ee3dbdbf3cfca6bd261ebb      1
     42 0x2584fcefec2fe5a4d3e3df5d6f946f7b356acef2      1
     43 0x274c821ddfa5fd5ce2c054f6fe854891e9f49e56      1
     44 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     45 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     46 0x2e1bea74e3068404ae07487b95da77c7829641cd      1
     47 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     48 0x3039fb04dd8c5d0d66493df168cd7aba92c57154      1
     49 0x31963b060d71ee24a6d458b75aa85e63b99bd7fb      1
     50 0x31f5afe73e35de36ec0b99ab683883af4dfc8dce      1
     51 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     52 0x3346857e5717918801428184d4e63dfb9f38e51f      1
     53 0x3489164bbc9fe2508ad9ae0c4f0d51556fa40f91      1
     54 0x35946c51a2bb1dcc7d2b417ef23d3e982f121330      1
     55 0x3686a4b272c646ef6fbe34377337d95db7356e63      1
     56 0x3a88bc828083eadea53f87520250e092b72350f2      1
     57 0x3a94ec43996178f7013988358f58baf18a4cc707      1
     58 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     59 0x3adde9c976371a06c95b70030b31fc37117241b4      1
     60 0x3ba9a34e2aed69b23e6e5ede3ac5546dae4abedf      1
     61 0x3c1cf8e31e7b6e46baa9856c8d52f8f10562389f      1
     62 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     63 0x3da7b8a0353fb926168a2464ffc4160be49b903b      1
     64 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     65 0x3deed956b999b83361b85bff31d388c35125411d      1
     66 0x40b767d119dbc81547cc13c6c55b6ed40a6506f1      1
     67 0x42689518d72a0fdbc34cfe30aa36d89ed8beb5bb      1
     68 0x44e9d011df8741be2493e99297e9ad67bb1aa85b      1
     69 0x4639e37f75a4599fe22371d618c9336da47cad1a      1
     70 0x46ae568842e3f3d2bb7e2a4f6ca8e38692c6ab86      1
     71 0x47279fc734466ee8acfa50aed553ff79770c8779      1
     72 0x47a931aba7dca4c8a61458797ed58f15170efe90      1
     73 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
     74 0x4835b48fc847e51cd63cca309250c3cd6f4d285e      1
     75 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     76 0x49ad224c1d54596c55d0c223b61f826dad29e7a7      1
     77 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     78 0x4cf01a807af3fbb5fd29a5da34136ba3df0f932a      1
     79 0x4dd5a4d1fcb6f8938aa8ad070aefe1b19e8f9c0c      1
     80 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
     81 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
     82 0x5178b38849eb9200ad681309ee4a6d5b00c23e51      1
     83 0x5316909738e83c293b2f8cb445a4ec5e9550bece      1
     84 0x5443415d3807f684095525b9f8f6b892389a40eb      1
     85 0x55b97e94b29cd9bf491fe63011cc23dae6392628      1
     86 0x55e9bf66235fb1461e5d8d7216f1a4bdbe85824f      1
     87 0x563c0e9846812a0678fe028052f8b3c857d85032      1
     88 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
     89 0x57f1c8d52bafcbddcaaa71be11fea6e48e11af11      1
     90 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     91 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
     92 0x5b046272cb9fde317ab73a836546e52b1f2d81f3      1
     93 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     94 0x5f22dbd716ec054a9802d1d0632902398d097c36      1
     95 0x605e2886d8e4632b8acf819af733aa2cbdc94583      1
     96 0x6177adacfde59c3ace55356250b6504488ac513d      1
     97 0x61c5fa0c1bed50eab527e69608d1da11bd4d1bf3      1
     98 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
     99 0x63985b16931a9fe854fe63d3584b24f1ed082f87      1
    100 0x6439e9006c8da5f686eafa4718b295f531b38083      1
    101 0x64c0be5f58b4da7253463043359432e032e3edcc      1
    102 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    103 0x66ea2d335291a5afa8d3522eef6adfc3f8746e16      1
    104 0x67ea5cbe1059d53e4580db289721b75b0ca963e3      1
    105 0x692d1fce318f98865203a256b1024e070295c6f4      1
    106 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    107 0x69e4767d4c63dbde5ed038eb52da33cf1ab9e4fe      1
    108 0x6a6873c7d9cfa4f79e5651f3676cedae78d4a649      1
    109 0x6a6d335d1ca6c4539b7cee6476b706685f97bc78      1
    110 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    111 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    112 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    113 0x6d745b5bfe909c7ca49499e58db787021537c9cf      1
    114 0x6dd5d07f6868d5db3b055791b9dce502d0f159c8      1
    115 0x6ebeba28d04a68bbb137380cf22edd38fd993dfb      1
    116 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    117 0x6f1cc221cd5c49331374908bf9d0f82ed4619423      1
    118 0x6f2148034c9615c0dc0d06589caf73d526d000e4      1
    119 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    120 0x725a70b1847d1bf3fbb7dcaf43880a9343f308be      1
    121 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    122 0x7465ce02a9b3eac18b580ebfe87c4ac9fbb2e628      1
    123 0x7616401731473d71e2dece59cb386610d553e546      1
    124 0x768057610a07396e05046af01e7c9b912e4a81f3      1
    125 0x77039399801f462b4ed13444a266b16355c471bf      1
    126 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    127 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    128 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
    129 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    130 0x79dc911ba6f8424a4b6158ea0b24168a8e0e0fe4      1
    131 0x7af2e406bcc85580f2994f6cee959b280c6e0c32      1
    132 0x7b50b1f2634fa38d078eb067a4f8f22a666b49a2      1
    133 0x7be90f10f9c6cd02f32401c4929e5a5dbaa0a51b      1
    134 0x7c2f06ab61a94fe9760cc8d9ad35a33c0472cb8e      1
    135 0x7c59a1ddd7b8e5ba683941ea67b20b93864ad8f9      1
    136 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    137 0x7d422cf705af9ea93919b1a6e843b2f9f387562d      1
    138 0x7dab31a034ef37229a53d270dd973e4f559665b3      1
    139 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    140 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    141 0x7fdda177cc70b317d90d6259e6d919f9ae60ab48      1
    142 0x8030e495ea55d8ec8ae1e10bf7498bf7f0eac1aa      1
    143 0x8099dcb5d0fbe0c0b45f10c4eba244428a4a725c      1
    144 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    145 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    146 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    147 0x8497277c9339170a0420e86da8352e0c084624cd      1
    148 0x856d040ffd5e86860d8025521fba6633c4006b52      1
    149 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    150 0x87bf447ac29ba9498e5c0859513c39a0931f303a      1
    151 0x8912f371133b8474229e52e71174d3c7dfca6e06      1
    152 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
    153 0x8b7d031fb0d8aab5a985583c6c56c4fffab27ee7      1
    154 0x8df65a9e58fa621c404f6353181d9f2cd9a0359d      1
    155 0x8e7a0b4c6550e22880081451c4f75e497e037e3e      1
    156 0x8e8f3e4891f87c42b6f512b38be9bc7583198b3e      1
    157 0x8f06975b0c3cc46087134339fb22ee2d46d2106d      1
    158 0x909d3c000baa83a64811ec0f41da55ec0ebb8a67      1
    159 0x92aebbcb31a62b83bc96a9691da2d6d6f0f1ce85      1
    160 0x92c91e0ae69264c52b630ad522eb9781cda31d82      1
    161 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    162 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
    163 0x9a78c0913396f8607eac7c4ae211a9138c2a1fac      1
    164 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    165 0x9b717cf7313315f27e8fa93c822f3e9cda07e8b2      1
    166 0x9d2e431a3010d9f3ac3c1f5ace093ac1f1d102b7      1
    167 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
    168 0x9d75d2f3c33f93468689fc2ba0dddbca831d5b8d      1
    169 0xa0bd213eba4a6117d2ad12d86dee6876eb5f034c      1
    170 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    171 0xa14beaa3bcf95d4e13a8aff1b34aa4468edb38ba      1
    172 0xa283dfd91be8d638ea7e8d4800c20980ee2dda68      1
    173 0xa3eac0016f6581ac34768c0d4b99ddcd88071c3c      1
    174 0xa43711f5dcc3559a1e07ec124fd167e26c9bad43      1
    175 0xa454daff2609603b169e6290fe69667705ed11fb      1
    176 0xa5430730f12f1128bf10dfba38c8e00bc4d90eea      1
    177 0xa5dce742f3775059d9406342d2ecbb0ff8e3a3c2      1
    178 0xa7f66d3ff024e639191c89170463b6ac4ff1478d      1
    179 0xa86cd6bfe453ad3bd3bfbb263d223a1a39eebeab      1
    180 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    181 0xa96cbc4c3651b25aebf9ec28d478781bc366bac7      1
    182 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    183 0xabb77d443c8f50c3f16cf8ae126941ba9153d9fd      1
    184 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    185 0xace1e41547899bda3db142ca28bc6f6de92e362b      1
    186 0xace3898b6e111f8154234e22d248cc3165cee849      1
    187 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    188 0xaed0ab8695fbd707424bd07a2e8ae1c6d7c94a8d      1
    189 0xaf4a749f3e218a67e40fbc295ba58f93c890a27d      1
    190 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    191 0xb1f882290617f9bdb50dbf970e196839f74735cb      1
    192 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    193 0xb79c26fafaafeb2835ff175d025db1b9deeedf5e      1
    194 0xb8ad88b9d89aca9261ddcbfee62177ed86d99454      1
    195 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
    196 0xbd440f1465cb5133b98dc6e8f6514feba6aa8a54      1
    197 0xbfe6ab779517544df703159ea41e7d8a71db6ef0      1
    198 0xc0858cb8bb6ffbada00da01a74193a8ff4bb64b9      1
    199 0xc08c48349322d72d2372aabea4ef6e9153960c3d      1
    200 0xc15add7eb1ba708bc7189cef6a4c47200b77a52b      1
    201 0xc1855da46c382600e1571484d0880d4967e85aee      1
    202 0xc25461af5e224b7aceeaf0eff443a239fd5040e6      1
    203 0xc2de42cf18e7b5d9774050eba2d5e2a33bf3bf63      1
    204 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
    205 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    206 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    207 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    208 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    209 0xc6f6c89f7f297b5230d2fd028ac06f0677c1857b      1
    210 0xc7d3e2ad7a2c712b355688b0022580542769d81e      1
    211 0xc8301c772bc161d261acca1de406012afbbcf854      1
    212 0xca970bc37e2804289fcf651123afdf919d6da235      1
    213 0xcb3408bad3192777e561b362e8467c213231ef9f      1
    214 0xcb5d4b21fed74edc68782fe07d084759c9c54151      1
    215 0xccd021ac40448e74641c34e71aaf617afa951ba3      1
    216 0xcce5ad053b8a02c2ecaa8c00412ef9915339542f      1
    217 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    218 0xcf0df56e67f469f390e5c458c9debd875b6a5eb3      1
    219 0xd050c97e76f882187c781feb2e74345227e2f710      1
    220 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    221 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    222 0xd2628fb21499c690f5015af5df410a6fc72df72f      1
    223 0xd407e5b3f487b4b74c4f3f5c8a10a801285146a2      1
    224 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    225 0xd578a6d0958537c39cd9d6599ab83ed7f410f93b      1
    226 0xd70676cfa67abc65a8c39f049d62eea30e3080cf      1
    227 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    228 0xd7c45ea3e2eff97007d7c9f96d37141167b3fa7b      1
    229 0xd8478053a45bfcdc7a8411b27c7329274c49de05      1
    230 0xd849843971653d1d3c8d40864ab4f16080fdcbf4      1
    231 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    232 0xda86d12e752631964895e2ae91c8fc8465967d2a      1
    233 0xdaf9472ecbfab430969ffe4b4fae2ab546198391      1
    234 0xdbc50b04023fd956f13d3f2625df7fbb022b996e      1
    235 0xdc29516f75bb6d8e7e76566062cd90f8c3087072      1
    236 0xdc9f06ec53b831934a1425e769ac498e3fef06cd      1
    237 0xdd0f08dad5cb2a920f6f9a72b2d9f44699df3a0a      1
    238 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    239 0xe1268b49d3e4e559f466239df4bee1d5cae04356      1
    240 0xe6ba7ce1bbd7b811d891c2d4c09d7295bb96752c      1
    241 0xe72c4bea4b5f5caabeaca6cd38894f8ede1f2e16      1
    242 0xe88605c4d8a7ba1e4a1c897d0e82c6b27311ccef      1
    243 0xe99b70874dd7785e7d88b0a629afe80ac1e19634      1
    244 0xeb68669d321e1459900d83595818ce1313a4d90f      1
    245 0xebfd394de050a10b0a9d8408ef395aaa8d269de8      1
    246 0xec63e88ba3d5482cbfa59872d2d889ba57796e17      1
    247 0xec876db33e7083d9ddcaf210a92c5b0182f8b2f8      1
    248 0xecba5f51925e6ccec26da38dcd7d5305f6bdfbcb      1
    249 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    250 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    251 0xef77bc2f8cb4ec186c277089eb164c1aa7a1e14a      1
    252 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    253 0xf14afa6a2960a3e79a9a1d617405907f67a7f9d4      1
    254 0xf1da6e2d387e9da611dac8a7fc587eaa4b010013      1
    255 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    256 0xf22742f06e4f6d68a8d0b49b9f270bb56affab38      1
    257 0xf2996f01814cc1fb1d0aa776678668fd0f91b795      1
    258 0xf3860788d1597cecf938424baabe976fac87dc26      1
    259 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
    260 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    261 0xf4d63913a748a6bbd99fbca027c58f3e9ee72a0c      1
    262 0xf86a588f3604e3b12899f710e3d572f76ffb94b8      1
    263 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    264 0xfba7737aea0969ce95cc7f1879b58e8fb8c16836      1
    265 0xfc0344564a36d3f572beca9cdaf67ca123bd4b45      1
    266 0xfdb26046b3138f5a812fffe027527c0bf9c4d686      1
    267 0xfdd3980797820a22e1eda8460c2dec333a9c3ae2      1
    268 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

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
