
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "memes345.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:2402        Length:2402        Min.   :1   Length:2402       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:2402       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16375369 # https://etherscan.io/block/16375369
block_hash <- "0x81a9ffed35930cc90eb9c350014ef299c28db217130fb48654603aa625c3bdde"
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

allow_memes1       <- pick(snapshot, contracts=c("memes1"), address_pick=100)
allow_memes2       <- pick(snapshot, contracts=c("memes2"), address_pick=100)
allow_memes3       <- pick(snapshot, contracts=c("memes3"), address_pick=50)
```

## Allow Memes1

``` r
c(allow_memes1) %>%
tally() %T>%
readr::write_csv(file="allow_memes1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x0387876a09a45581109eecab2ff4ac049e76ba37      1
      3 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
      4 0x078cc4caa131eb9d6d3e31847946d5f37fd3f58b      1
      5 0x08f8070f1a17791dcfda507d2e1a47bd78b6cdc6      1
      6 0x0c28d263fccf3ed25a48ddcf6561dd9cccd791b7      1
      7 0x0cbe1fba05102c34365a742af159efc5a93d1a68      1
      8 0x0ce6a432eaf0fa856a7c774170999db207748240      1
      9 0x0f22657ddcf96c4d8e7cd22f0ab18fbfb579e551      1
     10 0x0f7a8ebabea76a2ed2fda765430c060b6d8c5d57      1
     11 0x118d6f5d61a9d881d452510f29c703abe9d80cdc      1
     12 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     13 0x19e3775d47d63e1802577aec70189e7e3d6ac17b      1
     14 0x205a7e3b7db1f84b89e3098e33ecd6377dd3cb50      1
     15 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     16 0x270ee8f5a362832b21569a0c1afa38798a9dbf69      1
     17 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
     18 0x28d2e96e6a94a37bdecb82579a4e01e873bc1d09      1
     19 0x2e0c7eb1da51d3b2bcf6d4a7722e044b92da3f58      1
     20 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
     21 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     22 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     23 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     24 0x334fceaed101ce84d166c9fca48573495a33f8f4      1
     25 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     26 0x340ee74b7257c6b11b7bf47fd279558ea9e143f8      1
     27 0x35837d1f64e15e537bdc4adf151f20ea624632a9      1
     28 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
     29 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     30 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     31 0x432ee7ac7b80d7c5178fba2ee910482f7a6abd24      1
     32 0x46abfa031be839b1599513887a27a403e8d6598d      1
     33 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
     34 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     35 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
     36 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     37 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
     38 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     39 0x56a061f4ef706e1f6dcdbcf7e10e4340c1d99bbd      1
     40 0x5a3a1461310884df894b7e973305f690fc5779d0      1
     41 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
     42 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
     43 0x60e5299f49cdbc13d152323105af462071b22c87      1
     44 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
     45 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
     46 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
     47 0x6bb59e15545dc9ab0949410cb3d8402ced7fef98      1
     48 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
     49 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
     50 0x72f04749426a2527b8b52e760eb9fd454f11cfbc      1
     51 0x73d05c2ea70dfc3b220444c94567dbc84bb0d24c      1
     52 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     53 0x765718a8e4a5562be34b122af259ce5a71372e12      1
     54 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
     55 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
     56 0x7ae4784a907460858231609f565bd9580f609b05      1
     57 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
     58 0x8037cfedb47d493a391dad76c4f60b8927cb8108      1
     59 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
     60 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
     61 0x896b94f4f27f12369698c302e2049cae86936bbb      1
     62 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
     63 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
     64 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
     65 0x9bc66bb0f94a38e55809dcf7eadfd6bf98d6e184      1
     66 0xa222204acf1be4077d34102fab38a759060b77c2      1
     67 0xa73769aed346319287410811639ac3bec8464d55      1
     68 0xa76bda01601ebdd50512c2103a329db572d7a77a      1
     69 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
     70 0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708      1
     71 0xac1a04679039a1718d3820fbc254ce29269af784      1
     72 0xacc4b6f3ca8d59f631c1148dcaa52b9d7f5c819a      1
     73 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
     74 0xb391806676cec7fd36dc136dbd4097bde13e5b5d      1
     75 0xb5beebbfb568be3d5d7afc7c35cac5bc517a1fa4      1
     76 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
     77 0xbe9a677708192ae85e54fb38457b3b4f01c281cc      1
     78 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
     79 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
     80 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
     81 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
     82 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
     83 0xda4c79ccfdcbfa9128b1e328f77e5f9c6ad72a44      1
     84 0xdafa9e3dae493f0b9d6872eff4fda0f40d1b7488      1
     85 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
     86 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
     87 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
     88 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
     89 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
     90 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
     91 0xeb54ad6b8c3e2d43d72b6274627747ce5bfecb66      1
     92 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
     93 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
     94 0xf33f6c5302fee8dcaf6363f6862071092b3d56b0      1
     95 0xf4406f50666f4373bdad5017a9322f59d5e3691c      1
     96 0xf6b29a05b0023a8c219705fbe550b2999b94b1b4      1
     97 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
     98 0xfc61d7884b047f9f4bf56e984f773f1bd5d51480      1
     99 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    100 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

## Allow Memes2

``` r
c(allow_memes2) %>%
tally() %T>%
readr::write_csv(file="allow_memes2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x009f284bb658c55d292904ee733c50827dbb8e5a      1
      2 0x00baeaa84510584a6e4624964b75321ddb6766cc      1
      3 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      4 0x018f5cbe8a37b03f5bf733016fee38106d39713a      1
      5 0x02009370ff755704e9acbd96042c1ab832d6067e      1
      6 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
      7 0x05d23f26b6dafc7043fe94fbe85379e6bd0bcedc      1
      8 0x0763e484268042be6e5743c731a7adf47a48ec19      1
      9 0x07e34a9ccef73d5cc36d89d6767e3760658340de      1
     10 0x08fc70adf6b0950749b7647f67616589b1853a53      1
     11 0x0b358edd6e7eb4b38af3ddd7fcea775254602532      1
     12 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     13 0x0f615319d7ceed5801faf6b13c9034de9223a3ec      1
     14 0x11b7b38d5b96d02d6381e646ad8ca55db74dadc8      1
     15 0x15a415c7589efaf71345006d33bfa07a9a42c8c8      1
     16 0x19285d3163255c6e5d225dec48d3a5b797f381b7      1
     17 0x1a051968e421023d61a0afac659b6916784f251f      1
     18 0x1a5de5c215673a1d36650dadbb9eb2df04c0c7b7      1
     19 0x1da5331994e781ab0e2af9f85bfce2037a514170      1
     20 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     21 0x1f2dd54ce97965c6aee5d860118c326d68a21f63      1
     22 0x212caa051f36024a53ebc7817fd1f93e130d188d      1
     23 0x2135589bdf50c0226393c48514612145e2d3bedf      1
     24 0x2206445d241ccb7dae93b2d2acfc67f75b90fd76      1
     25 0x2227cb63299db1ac748347be2eecd8238efd593d      1
     26 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
     27 0x273396317b20d90dff0b69a8852ea912240828fe      1
     28 0x27d4fee2fbe69bc02be96d1e146abe0a29797424      1
     29 0x28b0ec2af8d6d30e02888b543652365180c6b3c9      1
     30 0x2c4376b9414fefecd89c9b770416f7516e8af863      1
     31 0x2d2052be503780c575629a22aa84990f9e38a7d5      1
     32 0x2db5a6d2a3d544f12a0c898ff25b1a18fcf9a554      1
     33 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     34 0x3135d041d207a3c3c4edfeba5cd1956439b5de38      1
     35 0x367a28e0cfbfa4626f2dbebd197046b2dfe67693      1
     36 0x36dd9e834ebcf94efda21b142577636b62d4770e      1
     37 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
     38 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     39 0x37dfaa62f0fe5e6ebe990ef1a0722f5279962e37      1
     40 0x38090615458c10d067dc19b5f1ccf7d4b4dd8bd6      1
     41 0x381334a57eb45ee79183ed126c7a688d600240e3      1
     42 0x3833a533f811a20b30b9694e8045ea575c0ae1f6      1
     43 0x3bd835333aad77686595d734ad5b80934d8b026e      1
     44 0x3c99c5157416d57db4ac590246bbf528211cf501      1
     45 0x3f701d50e68faae2b49fae9fc1262bbd360a9ea9      1
     46 0x418ea8e4ab433ae27390874a467a625f65f131b8      1
     47 0x428ae3bca203569a95065fe5ab0b27e053e83476      1
     48 0x44d6c9d5bce0831ec038d874dfb62464f40861b0      1
     49 0x4839ab5e2e84cc9765e38c818c8345c5517d5d19      1
     50 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
     51 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
     52 0x568b491fb2b4b9440f6d05e7723df0060fdaef1c      1
     53 0x5bc928bf0dab1e4a2ddd9e347b0f22e88026d76c      1
     54 0x5c509ce530f9ac85ffdf09c93c27654d0bc24b59      1
     55 0x5d3dd154c5e582b1b1ac90ebb0c05744038e32e0      1
     56 0x693fb4c126da8e126687b16c809fc19d2409d522      1
     57 0x78089705ed807984cf3edf9149c9369e8fabb9be      1
     58 0x7a4b4bd0dd0fca862e70d7eccf9e224b32dbcdfd      1
     59 0x7fc3dd768031af344ebd6cc27c449fa57a582917      1
     60 0x81787ecf8d91c1c6827f28e68e0ac3979a1263fd      1
     61 0x82f724dd0547f1962bf062539c33693154cae713      1
     62 0x867db38c83f4287a71ffd0cdfba6ffae93150ff7      1
     63 0x872dfa67e260fedcaf5c4b410d47b587b5a71e82      1
     64 0x87e5a5c79757d6e1d9c0586b3dcc7663e5095701      1
     65 0x884b9565d3a9d7265211f9354170a4f12ee2c4c9      1
     66 0x8b726cdb5e24207c4a6e23244d5b029026cbd23b      1
     67 0x8c1a163f87915654107148434fe68f60847abab6      1
     68 0x96e3baa591c9cf4d5f44dd44d02a3d4ce9290ac2      1
     69 0x9e027bc1bae517189285a207c69615d805670f7a      1
     70 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
     71 0xa41cd41474f7b59736da76b7fe12b9b8d9afb94f      1
     72 0xa4b2f8d5876ecbf976d7e983c9c9b418b21e11ae      1
     73 0xa5ff2bbc712fc114c29f3900ebb81b7713fe131f      1
     74 0xa610d3211a72ab1e1ecc6e6b19b3c357fe0b4289      1
     75 0xa67077ec8f947e8299335538167f625f3e407fff      1
     76 0xa8490d653e10e752d2e65b1ffb8a1789fdd19cf5      1
     77 0xa8ffc5696488443d290ad08f445b708cff68493b      1
     78 0xbc74c3adc2aa6a85bda3eca5b0e235ca08532772      1
     79 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
     80 0xc00b1fd8984cb568b451404074703c6a29bd91d0      1
     81 0xc3956f9e14e62bfe00672ac5a4b5d11f84d8b5b2      1
     82 0xc3df7d3ea6bc94cf674e22052a5d374ff08901bd      1
     83 0xcb954087ecc62651b29ee5e5bc9d06e85a557740      1
     84 0xd1e508c60d923cc9e93785da0bffe01710ccbac1      1
     85 0xd37d05655d8974e3d12e23cc1be06aaf4d4aece1      1
     86 0xd387a6e4e84a6c86bd90c158c6028a58cc8ac459      1
     87 0xd66a2ac715fd7628bbcbf49ac3dd4a0f3a5b847f      1
     88 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
     89 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
     90 0xe2cacedac44bd674d9c5e816422cbd603db9cc1c      1
     91 0xe6b4eafd769f2d29ec4bd2f197e393ddb7d75b84      1
     92 0xe7079eec020ddfc3f1c0abe1d946c55e6ed30eb3      1
     93 0xe74207386ac0dc2091da54f39b617ee0720efb69      1
     94 0xe79c263403b0071093bd7968ad1b6ea94d3bcf1c      1
     95 0xef646a7688ec76c3368b8df136e54394f6fdde9d      1
     96 0xf041c4f026547f7fbea6904f77bea16997024751      1
     97 0xf476cd75be8fdd197ae0b466a2ec2ae44da41897      1
     98 0xf54d81b2955832474a87a8dada4a042a05b83180      1
     99 0xf8f327e4c1cbcb0ef014031b84069b9d3579f42d      1
    100 0xfd5d8237da011a140949eb23517fbbb9edf142ce      1

## Allow Memes3

``` r
c(allow_memes3) %>%
tally() %T>%
readr::write_csv(file="allow_memes3.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0007fcf5590179336c826a60c80976623c7bbc29      1
     2 0x01d1cc95f80849a4564c9dae114255dba06a8d35      1
     3 0x027a9ce347d6964f9a9452299b04288e94c3b201      1
     4 0x029acd49d3aabc08402c84367c050dfd52b6c566      1
     5 0x057e2a10b1e72eaebbc7edf7f54fbc3762f975c1      1
     6 0x09bc19cd33faf805e55b708c2db3de87979b3950      1
     7 0x0a6f270ff37facd07aa85e48add3e73bb8101686      1
     8 0x0bd3292ab67d05187f0087c2e0ae010518d4e830      1
     9 0x0d083590c048a243e24a75e3a7c968145de25b44      1
    10 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
    11 0x10191ca151d7d7dd6bb2b3cbb358cb4a1e7e7361      1
    12 0x11083529652321166af1df9496a35a32e68f56ec      1
    13 0x1234311096a5d59d444d4ffc42b16b0ecb83b9c3      1
    14 0x15d4a76dd2253bef53d9346d840aff9fe385fb5b      1
    15 0x161a2562960f75d92fbbacc6abdc23fce9e571b8      1
    16 0x179504149c675376952500c65bd2f83052952541      1
    17 0x18fb2a2d9fa010dbcd33685a86ed209b821e5cbf      1
    18 0x251be313e35d620a36b148e1e301e7082e0d11c9      1
    19 0x274046b014ab894223268aef525f25277e073896      1
    20 0x28685cbf9b940923bd3de39232fb418243fb1e16      1
    21 0x2879707ec9b6c9c84d9c2512b64cd6ab36882cbc      1
    22 0x29341e12ec5fcc461ddd205f45eacd0caec2603d      1
    23 0x41859d1ce2b9683abc75df4c5a4f1ab7b38326ff      1
    24 0x42ca143584e8de7df35e6ae21502cc2c4e63ece8      1
    25 0x45c3d26097f723658860a5c8cec8e06ee23a9014      1
    26 0x47eabc96c1825826b7cb176673e4bb936ac709b2      1
    27 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
    28 0x4ec4f6f62f766977951733a018395fa8020112aa      1
    29 0x534b8531e362da97d808ba2ab3959fa2597bef11      1
    30 0x5b8660a36d3af77830a35355658bde886dad95b2      1
    31 0x7143f19d9df476227013260f015b06359918ef3f      1
    32 0x780ba6c53069f5fed88a59ae3e7a78710e889594      1
    33 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    34 0x938ec91427b4e8f0d35261ff1cc4dd04590ba642      1
    35 0x981af8b52d4a9d8a06b50ccd04b01b0dbe418eb0      1
    36 0x9e414d7ca84ec395070967223ac64c8264b90ecf      1
    37 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    38 0xa1b9b33aa2c318ea67d280cff079aac2d4784a13      1
    39 0xa2321ce998844cac8a2f67a481fa3e23e4c6f039      1
    40 0xa73bd72b4bf7875d4b4d53581bef3e0eadcd051b      1
    41 0xaef12b5976b89f1379f10df2be30fc22480277fe      1
    42 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
    43 0xc6a9f985af8737124aac0cc407a2ace271602261      1
    44 0xd8e93ccf41f079627f40573152bea5178041e1be      1
    45 0xdb56979106450cbbb6427b3b90b96af18a1f4546      1
    46 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    47 0xe48ab528f2b51fa68e22d57069cffafcd4aa2b6c      1
    48 0xed9788430cd53dd218c80063b2b788a99d95065d      1
    49 0xf6eb526bffa8d5036746df58fef23fb091739c44      1
    50 0xf829a4e55a73e7015b0425a952c06d0a886a744b      1

## Allow Memes345

``` r
c(allow_memes1,allow_memes2,allow_memes3) %>%
tally() %T>%
readr::write_csv(file="allow_memes345.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 250 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007fcf5590179336c826a60c80976623c7bbc29      1
      2 0x009f284bb658c55d292904ee733c50827dbb8e5a      1
      3 0x00baeaa84510584a6e4624964b75321ddb6766cc      1
      4 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      5 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      6 0x018f5cbe8a37b03f5bf733016fee38106d39713a      1
      7 0x01d1cc95f80849a4564c9dae114255dba06a8d35      1
      8 0x02009370ff755704e9acbd96042c1ab832d6067e      1
      9 0x027a9ce347d6964f9a9452299b04288e94c3b201      1
     10 0x029acd49d3aabc08402c84367c050dfd52b6c566      1
     11 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
     12 0x0387876a09a45581109eecab2ff4ac049e76ba37      1
     13 0x057e2a10b1e72eaebbc7edf7f54fbc3762f975c1      1
     14 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
     15 0x05d23f26b6dafc7043fe94fbe85379e6bd0bcedc      1
     16 0x0763e484268042be6e5743c731a7adf47a48ec19      1
     17 0x078cc4caa131eb9d6d3e31847946d5f37fd3f58b      1
     18 0x07e34a9ccef73d5cc36d89d6767e3760658340de      1
     19 0x08f8070f1a17791dcfda507d2e1a47bd78b6cdc6      1
     20 0x08fc70adf6b0950749b7647f67616589b1853a53      1
     21 0x09bc19cd33faf805e55b708c2db3de87979b3950      1
     22 0x0a6f270ff37facd07aa85e48add3e73bb8101686      1
     23 0x0b358edd6e7eb4b38af3ddd7fcea775254602532      1
     24 0x0bd3292ab67d05187f0087c2e0ae010518d4e830      1
     25 0x0c28d263fccf3ed25a48ddcf6561dd9cccd791b7      1
     26 0x0cbe1fba05102c34365a742af159efc5a93d1a68      1
     27 0x0ce6a432eaf0fa856a7c774170999db207748240      1
     28 0x0d083590c048a243e24a75e3a7c968145de25b44      1
     29 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     30 0x0f22657ddcf96c4d8e7cd22f0ab18fbfb579e551      1
     31 0x0f615319d7ceed5801faf6b13c9034de9223a3ec      1
     32 0x0f7a8ebabea76a2ed2fda765430c060b6d8c5d57      1
     33 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
     34 0x10191ca151d7d7dd6bb2b3cbb358cb4a1e7e7361      1
     35 0x11083529652321166af1df9496a35a32e68f56ec      1
     36 0x118d6f5d61a9d881d452510f29c703abe9d80cdc      1
     37 0x11b7b38d5b96d02d6381e646ad8ca55db74dadc8      1
     38 0x1234311096a5d59d444d4ffc42b16b0ecb83b9c3      1
     39 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     40 0x15a415c7589efaf71345006d33bfa07a9a42c8c8      1
     41 0x15d4a76dd2253bef53d9346d840aff9fe385fb5b      1
     42 0x161a2562960f75d92fbbacc6abdc23fce9e571b8      1
     43 0x179504149c675376952500c65bd2f83052952541      1
     44 0x18fb2a2d9fa010dbcd33685a86ed209b821e5cbf      1
     45 0x19285d3163255c6e5d225dec48d3a5b797f381b7      1
     46 0x19e3775d47d63e1802577aec70189e7e3d6ac17b      1
     47 0x1a051968e421023d61a0afac659b6916784f251f      1
     48 0x1a5de5c215673a1d36650dadbb9eb2df04c0c7b7      1
     49 0x1da5331994e781ab0e2af9f85bfce2037a514170      1
     50 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     51 0x1f2dd54ce97965c6aee5d860118c326d68a21f63      1
     52 0x205a7e3b7db1f84b89e3098e33ecd6377dd3cb50      1
     53 0x212caa051f36024a53ebc7817fd1f93e130d188d      1
     54 0x2135589bdf50c0226393c48514612145e2d3bedf      1
     55 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     56 0x2206445d241ccb7dae93b2d2acfc67f75b90fd76      1
     57 0x2227cb63299db1ac748347be2eecd8238efd593d      1
     58 0x251be313e35d620a36b148e1e301e7082e0d11c9      1
     59 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
     60 0x270ee8f5a362832b21569a0c1afa38798a9dbf69      1
     61 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
     62 0x273396317b20d90dff0b69a8852ea912240828fe      1
     63 0x274046b014ab894223268aef525f25277e073896      1
     64 0x27d4fee2fbe69bc02be96d1e146abe0a29797424      1
     65 0x28685cbf9b940923bd3de39232fb418243fb1e16      1
     66 0x2879707ec9b6c9c84d9c2512b64cd6ab36882cbc      1
     67 0x28b0ec2af8d6d30e02888b543652365180c6b3c9      1
     68 0x28d2e96e6a94a37bdecb82579a4e01e873bc1d09      1
     69 0x29341e12ec5fcc461ddd205f45eacd0caec2603d      1
     70 0x2c4376b9414fefecd89c9b770416f7516e8af863      1
     71 0x2d2052be503780c575629a22aa84990f9e38a7d5      1
     72 0x2db5a6d2a3d544f12a0c898ff25b1a18fcf9a554      1
     73 0x2e0c7eb1da51d3b2bcf6d4a7722e044b92da3f58      1
     74 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
     75 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     76 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     77 0x3135d041d207a3c3c4edfeba5cd1956439b5de38      1
     78 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     79 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     80 0x334fceaed101ce84d166c9fca48573495a33f8f4      1
     81 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     82 0x340ee74b7257c6b11b7bf47fd279558ea9e143f8      1
     83 0x35837d1f64e15e537bdc4adf151f20ea624632a9      1
     84 0x367a28e0cfbfa4626f2dbebd197046b2dfe67693      1
     85 0x36dd9e834ebcf94efda21b142577636b62d4770e      1
     86 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
     87 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
     88 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     89 0x37dfaa62f0fe5e6ebe990ef1a0722f5279962e37      1
     90 0x38090615458c10d067dc19b5f1ccf7d4b4dd8bd6      1
     91 0x381334a57eb45ee79183ed126c7a688d600240e3      1
     92 0x3833a533f811a20b30b9694e8045ea575c0ae1f6      1
     93 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     94 0x3bd835333aad77686595d734ad5b80934d8b026e      1
     95 0x3c99c5157416d57db4ac590246bbf528211cf501      1
     96 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     97 0x3f701d50e68faae2b49fae9fc1262bbd360a9ea9      1
     98 0x41859d1ce2b9683abc75df4c5a4f1ab7b38326ff      1
     99 0x418ea8e4ab433ae27390874a467a625f65f131b8      1
    100 0x428ae3bca203569a95065fe5ab0b27e053e83476      1
    101 0x42ca143584e8de7df35e6ae21502cc2c4e63ece8      1
    102 0x432ee7ac7b80d7c5178fba2ee910482f7a6abd24      1
    103 0x44d6c9d5bce0831ec038d874dfb62464f40861b0      1
    104 0x45c3d26097f723658860a5c8cec8e06ee23a9014      1
    105 0x46abfa031be839b1599513887a27a403e8d6598d      1
    106 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
    107 0x47eabc96c1825826b7cb176673e4bb936ac709b2      1
    108 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    109 0x4839ab5e2e84cc9765e38c818c8345c5517d5d19      1
    110 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
    111 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
    112 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
    113 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    114 0x4ec4f6f62f766977951733a018395fa8020112aa      1
    115 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
    116 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
    117 0x534b8531e362da97d808ba2ab3959fa2597bef11      1
    118 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
    119 0x568b491fb2b4b9440f6d05e7723df0060fdaef1c      1
    120 0x56a061f4ef706e1f6dcdbcf7e10e4340c1d99bbd      1
    121 0x5a3a1461310884df894b7e973305f690fc5779d0      1
    122 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
    123 0x5b8660a36d3af77830a35355658bde886dad95b2      1
    124 0x5bc928bf0dab1e4a2ddd9e347b0f22e88026d76c      1
    125 0x5c509ce530f9ac85ffdf09c93c27654d0bc24b59      1
    126 0x5d3dd154c5e582b1b1ac90ebb0c05744038e32e0      1
    127 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
    128 0x60e5299f49cdbc13d152323105af462071b22c87      1
    129 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
    130 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
    131 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    132 0x693fb4c126da8e126687b16c809fc19d2409d522      1
    133 0x6bb59e15545dc9ab0949410cb3d8402ced7fef98      1
    134 0x7143f19d9df476227013260f015b06359918ef3f      1
    135 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    136 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
    137 0x72f04749426a2527b8b52e760eb9fd454f11cfbc      1
    138 0x73d05c2ea70dfc3b220444c94567dbc84bb0d24c      1
    139 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    140 0x765718a8e4a5562be34b122af259ce5a71372e12      1
    141 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    142 0x78089705ed807984cf3edf9149c9369e8fabb9be      1
    143 0x780ba6c53069f5fed88a59ae3e7a78710e889594      1
    144 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
    145 0x7a4b4bd0dd0fca862e70d7eccf9e224b32dbcdfd      1
    146 0x7ae4784a907460858231609f565bd9580f609b05      1
    147 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
    148 0x7fc3dd768031af344ebd6cc27c449fa57a582917      1
    149 0x8037cfedb47d493a391dad76c4f60b8927cb8108      1
    150 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
    151 0x81787ecf8d91c1c6827f28e68e0ac3979a1263fd      1
    152 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    153 0x82f724dd0547f1962bf062539c33693154cae713      1
    154 0x867db38c83f4287a71ffd0cdfba6ffae93150ff7      1
    155 0x872dfa67e260fedcaf5c4b410d47b587b5a71e82      1
    156 0x87e5a5c79757d6e1d9c0586b3dcc7663e5095701      1
    157 0x884b9565d3a9d7265211f9354170a4f12ee2c4c9      1
    158 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    159 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    160 0x8b726cdb5e24207c4a6e23244d5b029026cbd23b      1
    161 0x8c1a163f87915654107148434fe68f60847abab6      1
    162 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    163 0x938ec91427b4e8f0d35261ff1cc4dd04590ba642      1
    164 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
    165 0x96e3baa591c9cf4d5f44dd44d02a3d4ce9290ac2      1
    166 0x981af8b52d4a9d8a06b50ccd04b01b0dbe418eb0      1
    167 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
    168 0x9bc66bb0f94a38e55809dcf7eadfd6bf98d6e184      1
    169 0x9e027bc1bae517189285a207c69615d805670f7a      1
    170 0x9e414d7ca84ec395070967223ac64c8264b90ecf      1
    171 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    172 0xa1b9b33aa2c318ea67d280cff079aac2d4784a13      1
    173 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
    174 0xa222204acf1be4077d34102fab38a759060b77c2      1
    175 0xa2321ce998844cac8a2f67a481fa3e23e4c6f039      1
    176 0xa41cd41474f7b59736da76b7fe12b9b8d9afb94f      1
    177 0xa4b2f8d5876ecbf976d7e983c9c9b418b21e11ae      1
    178 0xa5ff2bbc712fc114c29f3900ebb81b7713fe131f      1
    179 0xa610d3211a72ab1e1ecc6e6b19b3c357fe0b4289      1
    180 0xa67077ec8f947e8299335538167f625f3e407fff      1
    181 0xa73769aed346319287410811639ac3bec8464d55      1
    182 0xa73bd72b4bf7875d4b4d53581bef3e0eadcd051b      1
    183 0xa76bda01601ebdd50512c2103a329db572d7a77a      1
    184 0xa8490d653e10e752d2e65b1ffb8a1789fdd19cf5      1
    185 0xa8ffc5696488443d290ad08f445b708cff68493b      1
    186 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
    187 0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708      1
    188 0xac1a04679039a1718d3820fbc254ce29269af784      1
    189 0xacc4b6f3ca8d59f631c1148dcaa52b9d7f5c819a      1
    190 0xaef12b5976b89f1379f10df2be30fc22480277fe      1
    191 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    192 0xb391806676cec7fd36dc136dbd4097bde13e5b5d      1
    193 0xb5beebbfb568be3d5d7afc7c35cac5bc517a1fa4      1
    194 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
    195 0xbc74c3adc2aa6a85bda3eca5b0e235ca08532772      1
    196 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    197 0xbe9a677708192ae85e54fb38457b3b4f01c281cc      1
    198 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    199 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    200 0xc00b1fd8984cb568b451404074703c6a29bd91d0      1
    201 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    202 0xc3956f9e14e62bfe00672ac5a4b5d11f84d8b5b2      1
    203 0xc3df7d3ea6bc94cf674e22052a5d374ff08901bd      1
    204 0xc6a9f985af8737124aac0cc407a2ace271602261      1
    205 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    206 0xcb954087ecc62651b29ee5e5bc9d06e85a557740      1
    207 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    208 0xd1e508c60d923cc9e93785da0bffe01710ccbac1      1
    209 0xd37d05655d8974e3d12e23cc1be06aaf4d4aece1      1
    210 0xd387a6e4e84a6c86bd90c158c6028a58cc8ac459      1
    211 0xd66a2ac715fd7628bbcbf49ac3dd4a0f3a5b847f      1
    212 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    213 0xd8e93ccf41f079627f40573152bea5178041e1be      1
    214 0xda4c79ccfdcbfa9128b1e328f77e5f9c6ad72a44      1
    215 0xdafa9e3dae493f0b9d6872eff4fda0f40d1b7488      1
    216 0xdb56979106450cbbb6427b3b90b96af18a1f4546      1
    217 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
    218 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
    219 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    220 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
    221 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    222 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    223 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    224 0xe2cacedac44bd674d9c5e816422cbd603db9cc1c      1
    225 0xe48ab528f2b51fa68e22d57069cffafcd4aa2b6c      1
    226 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    227 0xe6b4eafd769f2d29ec4bd2f197e393ddb7d75b84      1
    228 0xe7079eec020ddfc3f1c0abe1d946c55e6ed30eb3      1
    229 0xe74207386ac0dc2091da54f39b617ee0720efb69      1
    230 0xe79c263403b0071093bd7968ad1b6ea94d3bcf1c      1
    231 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    232 0xeb54ad6b8c3e2d43d72b6274627747ce5bfecb66      1
    233 0xed9788430cd53dd218c80063b2b788a99d95065d      1
    234 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    235 0xef646a7688ec76c3368b8df136e54394f6fdde9d      1
    236 0xf041c4f026547f7fbea6904f77bea16997024751      1
    237 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    238 0xf33f6c5302fee8dcaf6363f6862071092b3d56b0      1
    239 0xf4406f50666f4373bdad5017a9322f59d5e3691c      1
    240 0xf476cd75be8fdd197ae0b466a2ec2ae44da41897      1
    241 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    242 0xf6b29a05b0023a8c219705fbe550b2999b94b1b4      1
    243 0xf6eb526bffa8d5036746df58fef23fb091739c44      1
    244 0xf829a4e55a73e7015b0425a952c06d0a886a744b      1
    245 0xf8f327e4c1cbcb0ef014031b84069b9d3579f42d      1
    246 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    247 0xfc61d7884b047f9f4bf56e984f773f1bd5d51480      1
    248 0xfd5d8237da011a140949eb23517fbbb9edf142ce      1
    249 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    250 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

## Versioning

``` r
R.version$version.string
```

    [1] "R version 4.2.1 (2022-06-23)"

``` r
packageVersion("tidyverse")
```

    [1] '1.3.2'

``` r
packageVersion("magrittr")
```

    [1] '2.0.3'
