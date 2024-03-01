
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:295         Length:295         Min.   :1.000   Length:295        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.119                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:295        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19296069 # https://etherscan.io/block/19296069
block_hash <- "0x914cdbc3d69e5e6c6b294016df5a23787416e7850fdeb98ab03a035ec682333e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4612 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SkullyBoy","PopularDemand","Foundation"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("SkullyBoyBiddersEditions","SolitaryConfinementEditions","SkullyBoyEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SkullyBoy","PopularDemand","Foundation","SkullyBoyBiddersEditions","SolitaryConfinementEditions","SkullyBoyEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 13 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     2 0x257d9128e4b8abe05ca3b045a216ed86bcf08af4      1
     3 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     4 0x3eb18413d536d7ab03320300598de457fa078ab2      1
     5 0x3febb7a93aa9865c7bc1ae14dcc94ff0a7132d46      1
     6 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     7 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     8 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     9 0x74401b66f746630ccee98689b1a20e0149fbccab      1
    10 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    11 0x90a8734076d7a7db4303fed64cf46467069f11d8      1
    12 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    13 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 113 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      2 0x059bf624d9e77c777d96389d058011d6904184cf      1
      3 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      4 0x0c57daae59367c1b26cd337fdc0b69356f31ffa9      1
      5 0x0f3c76ade30adf61b64d01154911308491784dfe      1
      6 0x191981c46b7d1aa34641e8415246d603035ae9bb      1
      7 0x1db6dc362abe8ebee595cc08e16b3473bae60a4d      1
      8 0x1dc301854bfecb4ba03eb417579585c2b8ea12c1      1
      9 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     10 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     11 0x2493c86b62e8ff26208399144817ef2898c59460      1
     12 0x262456d9a98537f4706324666b28bfa4e9d23446      1
     13 0x29b79ad1730dc87b5923d47554436501b5d05b84      1
     14 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     15 0x2ed0d6195c82d5ce4fee27121000851014260096      1
     16 0x2ff43dcaeda13f4f08c82bece23e974c807c08d3      1
     17 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     18 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     19 0x41fb51437551fc18914e9e4dbb592ce051b01051      1
     20 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     21 0x459daeed4942f9f23b19c38a06f8629e9e45c2ae      1
     22 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     23 0x48af2bd40905f8d48d187ecb3c6bbc19ec21c795      1
     24 0x50e22dc24387f682bb7752192c0b19aaeb90ff5a      1
     25 0x52a98cc2f1c17acd12054e891213ae17d05e695d      1
     26 0x52f685ae7144f10747b015af9dcc6f4cfed5de81      1
     27 0x53a3dc78694c1ae7ff1e36c9b9a97f10a67ac1bb      1
     28 0x55eea5ec5e162003b0a51418f7dce3dfab7f7007      1
     29 0x57081166ff4a1f68e66a79cdf52333d27e2e47e5      1
     30 0x59208cbba4e46011629644aa607c0dc161a02d16      1
     31 0x5eb27c71969c678d21585c8a2f37d4df1ad9b6e7      1
     32 0x616396affaf05e1514a685acd757ffb9432e2c3e      1
     33 0x63a9dbce75413036b2b778e670aabd4493aaf9f3      1
     34 0x689221ed66c0e08a10e1d0d0ef16c06641b5b030      1
     35 0x6a2ebd6b63dc3f8ccd1f91292cefb07255e01c86      1
     36 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     37 0x6be309c18fbe511b6f7883b04d184992310d2ada      1
     38 0x6c916adfa8b5652040de09037af43d0267ce02d0      1
     39 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     40 0x715a64f3a6669e1de61c3fd9e1a3b29821ec5974      1
     41 0x71a618889fba977def3fba6b3545b6d4e841f9e1      1
     42 0x728a4dde804aedaf93ac839c9b0fce031e0361af      1
     43 0x74200a3ce998786f45591bcaff3daf5a37825dd0      1
     44 0x758747a83d5a7fdd4d6b905c01b37a900337fe9a      1
     45 0x782adafbf47a604f146af4a059908e946eae539f      1
     46 0x78af47e0e9bcb8c24e27b837b638a241b28d4619      1
     47 0x7b86a910a6c797316dda30b78098e331e08b9f34      1
     48 0x82f23de5a474fc904041c357576afe53c30dd250      1
     49 0x8699793e22403b355185d8ff76e7392f98aafa46      1
     50 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     51 0x87accabbdedce63379af9d246bb979a3a481534c      1
     52 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
     53 0x892f939e25c2507cb1fe4b89996437221916f644      1
     54 0x8c0d32e856e30b9fd0d11a60e6a691c1d94517d5      1
     55 0x92950e5f7169b2dfc923807ffd61de3cf0e8cb09      1
     56 0x992acb5ed157a1d99c1dec7d09a001f162532449      1
     57 0x9aa74ec75fbec40075a048e254c43aed324c33b6      1
     58 0xa3b11d1f06d71eaa9cd3d0142f08e7ace9b474ec      1
     59 0xa693fe70a4180c29d0ea522dbd4833dced5fb94a      1
     60 0xa81134646bf68807e16e80dc9778a314ed1a9747      1
     61 0xacf3fd0a56f8a9996b3d12419fc9f131388630ae      1
     62 0xaea3e4bd4a5250ec413e31b95126f3f997493a8b      1
     63 0xaf801d0210fe028a04496c4b3820ddf8d0b5bdc6      1
     64 0xb54539e92fc2af3632426c725191c16640f4b5d2      1
     65 0xb57efba074966fc499835996746acde35d7ad38e      1
     66 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
     67 0xb89d16beac18e513743baa6a6e09f60460367ac8      1
     68 0xb8c3e2e2a3465b74b7a57f8921a1c3f6e69302e2      1
     69 0xb8f42454f788ca52489ba732cebdd044204ce91c      1
     70 0xb955193b05aa57b2f568d110c24ba80dbec7da65      1
     71 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
     72 0xbd8e37d657ac90ce5afde8a050a6d828eb49209c      1
     73 0xbe09e43a57f2980998cedd08517eae7d5ae683bb      1
     74 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
     75 0xc0760166512c505547e0bd2196a42b40984a7670      1
     76 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
     77 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
     78 0xd0f6dd7606c2989443e733a5faec76130f80e49f      1
     79 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
     80 0xd364d56e2e77ab95491a26f1376d961a9a9692d7      1
     81 0xd75c9192bf8ce60e405494f11d59526ecdd275b4      1
     82 0xd8e696000055e89df638daee057d60959a50804f      1
     83 0xda0a7f503b792d793bbab43008f5c204edeb123a      1
     84 0xda59a5ec18a3bc48f1346a1a439aee253405f2a1      1
     85 0xda691580df0e5add3cf750940111ef05bd9457d7      1
     86 0xdbc30d47297b5360f5fb386e5791b6ca75ba54a9      1
     87 0xdf39a03857d9cd6d42727e55e4b06194806ed4ea      1
     88 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
     89 0xe3f27deff96fe178e87559f36cbf868b9e75967d      1
     90 0xe5b545c8f1c6a9ecbc53d78b3a2916fcf065bb43      1
     91 0xe74419fda5425b88fe288923f5df60a5cda057be      1
     92 0xe74abe3bc87a4cf34acfd7c548208700c0e669b0      1
     93 0xe7ba082a114b5b9049a4c9213e60755c20351f79      1
     94 0xe91a7012ec1c170021b9038dccbda484924c1354      1
     95 0xea1c7edf57c7302679807bd7de1d21501bd61c03      1
     96 0xebe326d8de3413f8132518dcfd45e6cbff7e5c27      1
     97 0xecc953efbd82d7dea4aa0f7bc3329ea615e0cff2      1
     98 0xed2a83cdd784ab16bbe6edf3d2af501b1f50a539      1
     99 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    100 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    101 0xf2439241881964006369c0e2377d45f3740f48a0      1
    102 0xf2e21450c87701d95d289aca6eef297fa74e231c      1
    103 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
    104 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    105 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    106 0xf52526c0db0c0877ae726b3c33622bb65c10a1e7      1
    107 0xf762c3993403d170adee1f58974215905887128c      1
    108 0xf7c59e093db10e1fa9c016d08823b62f83f6bffa      1
    109 0xf7f2475a0866ae4a8bb1c67b4e2277bd3319a101      1
    110 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    111 0xfded90a3b1348425577688866f798f94d77a0d02      1
    112 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    113 0xff745f093b4b32b6655ac66e57a7af645f8f9e8f      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 126 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
      2 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      3 0x059bf624d9e77c777d96389d058011d6904184cf      1
      4 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      5 0x0c57daae59367c1b26cd337fdc0b69356f31ffa9      1
      6 0x0f3c76ade30adf61b64d01154911308491784dfe      1
      7 0x191981c46b7d1aa34641e8415246d603035ae9bb      1
      8 0x1db6dc362abe8ebee595cc08e16b3473bae60a4d      1
      9 0x1dc301854bfecb4ba03eb417579585c2b8ea12c1      1
     10 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     11 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     12 0x2493c86b62e8ff26208399144817ef2898c59460      1
     13 0x257d9128e4b8abe05ca3b045a216ed86bcf08af4      1
     14 0x262456d9a98537f4706324666b28bfa4e9d23446      1
     15 0x29b79ad1730dc87b5923d47554436501b5d05b84      1
     16 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     17 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     18 0x2ed0d6195c82d5ce4fee27121000851014260096      1
     19 0x2ff43dcaeda13f4f08c82bece23e974c807c08d3      1
     20 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     21 0x3eb18413d536d7ab03320300598de457fa078ab2      1
     22 0x3febb7a93aa9865c7bc1ae14dcc94ff0a7132d46      1
     23 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     24 0x41fb51437551fc18914e9e4dbb592ce051b01051      1
     25 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     26 0x459daeed4942f9f23b19c38a06f8629e9e45c2ae      1
     27 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     28 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     29 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     30 0x48af2bd40905f8d48d187ecb3c6bbc19ec21c795      1
     31 0x50e22dc24387f682bb7752192c0b19aaeb90ff5a      1
     32 0x52a98cc2f1c17acd12054e891213ae17d05e695d      1
     33 0x52f685ae7144f10747b015af9dcc6f4cfed5de81      1
     34 0x53a3dc78694c1ae7ff1e36c9b9a97f10a67ac1bb      1
     35 0x55eea5ec5e162003b0a51418f7dce3dfab7f7007      1
     36 0x57081166ff4a1f68e66a79cdf52333d27e2e47e5      1
     37 0x59208cbba4e46011629644aa607c0dc161a02d16      1
     38 0x5eb27c71969c678d21585c8a2f37d4df1ad9b6e7      1
     39 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     40 0x616396affaf05e1514a685acd757ffb9432e2c3e      1
     41 0x63a9dbce75413036b2b778e670aabd4493aaf9f3      1
     42 0x689221ed66c0e08a10e1d0d0ef16c06641b5b030      1
     43 0x6a2ebd6b63dc3f8ccd1f91292cefb07255e01c86      1
     44 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     45 0x6be309c18fbe511b6f7883b04d184992310d2ada      1
     46 0x6c916adfa8b5652040de09037af43d0267ce02d0      1
     47 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     48 0x715a64f3a6669e1de61c3fd9e1a3b29821ec5974      1
     49 0x71a618889fba977def3fba6b3545b6d4e841f9e1      1
     50 0x728a4dde804aedaf93ac839c9b0fce031e0361af      1
     51 0x74200a3ce998786f45591bcaff3daf5a37825dd0      1
     52 0x74401b66f746630ccee98689b1a20e0149fbccab      1
     53 0x758747a83d5a7fdd4d6b905c01b37a900337fe9a      1
     54 0x782adafbf47a604f146af4a059908e946eae539f      1
     55 0x78af47e0e9bcb8c24e27b837b638a241b28d4619      1
     56 0x7b86a910a6c797316dda30b78098e331e08b9f34      1
     57 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     58 0x82f23de5a474fc904041c357576afe53c30dd250      1
     59 0x8699793e22403b355185d8ff76e7392f98aafa46      1
     60 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     61 0x87accabbdedce63379af9d246bb979a3a481534c      1
     62 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
     63 0x892f939e25c2507cb1fe4b89996437221916f644      1
     64 0x8c0d32e856e30b9fd0d11a60e6a691c1d94517d5      1
     65 0x90a8734076d7a7db4303fed64cf46467069f11d8      1
     66 0x92950e5f7169b2dfc923807ffd61de3cf0e8cb09      1
     67 0x992acb5ed157a1d99c1dec7d09a001f162532449      1
     68 0x9aa74ec75fbec40075a048e254c43aed324c33b6      1
     69 0xa3b11d1f06d71eaa9cd3d0142f08e7ace9b474ec      1
     70 0xa693fe70a4180c29d0ea522dbd4833dced5fb94a      1
     71 0xa81134646bf68807e16e80dc9778a314ed1a9747      1
     72 0xacf3fd0a56f8a9996b3d12419fc9f131388630ae      1
     73 0xaea3e4bd4a5250ec413e31b95126f3f997493a8b      1
     74 0xaf801d0210fe028a04496c4b3820ddf8d0b5bdc6      1
     75 0xb54539e92fc2af3632426c725191c16640f4b5d2      1
     76 0xb57efba074966fc499835996746acde35d7ad38e      1
     77 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
     78 0xb89d16beac18e513743baa6a6e09f60460367ac8      1
     79 0xb8c3e2e2a3465b74b7a57f8921a1c3f6e69302e2      1
     80 0xb8f42454f788ca52489ba732cebdd044204ce91c      1
     81 0xb955193b05aa57b2f568d110c24ba80dbec7da65      1
     82 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
     83 0xbd8e37d657ac90ce5afde8a050a6d828eb49209c      1
     84 0xbe09e43a57f2980998cedd08517eae7d5ae683bb      1
     85 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
     86 0xc0760166512c505547e0bd2196a42b40984a7670      1
     87 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
     88 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
     89 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
     90 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
     91 0xd0f6dd7606c2989443e733a5faec76130f80e49f      1
     92 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
     93 0xd364d56e2e77ab95491a26f1376d961a9a9692d7      1
     94 0xd75c9192bf8ce60e405494f11d59526ecdd275b4      1
     95 0xd8e696000055e89df638daee057d60959a50804f      1
     96 0xda0a7f503b792d793bbab43008f5c204edeb123a      1
     97 0xda59a5ec18a3bc48f1346a1a439aee253405f2a1      1
     98 0xda691580df0e5add3cf750940111ef05bd9457d7      1
     99 0xdbc30d47297b5360f5fb386e5791b6ca75ba54a9      1
    100 0xdf39a03857d9cd6d42727e55e4b06194806ed4ea      1
    101 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    102 0xe3f27deff96fe178e87559f36cbf868b9e75967d      1
    103 0xe5b545c8f1c6a9ecbc53d78b3a2916fcf065bb43      1
    104 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    105 0xe74abe3bc87a4cf34acfd7c548208700c0e669b0      1
    106 0xe7ba082a114b5b9049a4c9213e60755c20351f79      1
    107 0xe91a7012ec1c170021b9038dccbda484924c1354      1
    108 0xea1c7edf57c7302679807bd7de1d21501bd61c03      1
    109 0xebe326d8de3413f8132518dcfd45e6cbff7e5c27      1
    110 0xecc953efbd82d7dea4aa0f7bc3329ea615e0cff2      1
    111 0xed2a83cdd784ab16bbe6edf3d2af501b1f50a539      1
    112 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    113 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    114 0xf2439241881964006369c0e2377d45f3740f48a0      1
    115 0xf2e21450c87701d95d289aca6eef297fa74e231c      1
    116 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
    117 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    118 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    119 0xf52526c0db0c0877ae726b3c33622bb65c10a1e7      1
    120 0xf762c3993403d170adee1f58974215905887128c      1
    121 0xf7c59e093db10e1fa9c016d08823b62f83f6bffa      1
    122 0xf7f2475a0866ae4a8bb1c67b4e2277bd3319a101      1
    123 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    124 0xfded90a3b1348425577688866f798f94d77a0d02      1
    125 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    126 0xff745f093b4b32b6655ac66e57a7af645f8f9e8f      1

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
