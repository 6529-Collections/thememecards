
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance       contract        
     Length:176         Length:176         Min.   :1.00   Length:176        
     Class :character   Class :character   1st Qu.:1.00   Class :character  
     Mode  :character   Mode  :character   Median :1.00   Mode  :character  
                                           Mean   :1.04                     
                                           3rd Qu.:1.00                     
                                           Max.   :3.00                     
         name          
     Length:176        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21598869 # https://etherscan.io/block/21598869
block_hash <- "0xc0a8998cdc88b4c7d760f7b98300ad6521315a587a13f3a647efb331b038ef0c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4604 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","MidnightHour"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DeathEditions","MidnightHourContemplationsEditions","CyberMythical4Editions","CyberMythical3Editions","CyberMythical2Editions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","MidnightHour","DeathEditions","MidnightHourContemplationsEditions","CyberMythical4Editions","CyberMythical3Editions","CyberMythical2Editions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2afb4c1b3c2c4d93d71909afbf775358cb74f52b      1
    2 0x4949b46633b810bdd745b028062b30f6b647ec60      1
    3 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
    4 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 117 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00a58651879fd753ab58d639b633368859cda6e1      1
      2 0x01a8bc97c867c28eb3a1b0af5a12e761a7a6a8e3      1
      3 0x0523dbfa086483164bb773c920e6a868a5cd40a1      1
      4 0x05ee1ee157d9a1c62ee8a671ba2abeac0ae6f2d5      1
      5 0x060fb9f0d0529a80f5fb94ecad98df7df99ecb0a      1
      6 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
      7 0x0a834bbc8d2273c2416ef1cd7a4df735f60cd751      1
      8 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
      9 0x0fc5df1a76dac8a19d11d2ddcc5bbe4b79bba910      1
     10 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     11 0x11d3502407d152147ba2ca2cfe78fa8dc8e5ef70      1
     12 0x15d51e51caf5585a40cb965080098bfb68af3336      1
     13 0x17d369a86cdfa617c8d3b4bf487335dae4cfe9d4      1
     14 0x18e7df01107437323e4738222f9c8a9060616e08      1
     15 0x1a1995aece8077c0209f0064a47d7e8033b2aa4f      1
     16 0x1bec0e6266e97f0c7c42aa720959e46598dd8ce1      1
     17 0x1c028286e330a2facca02fb6bf91136c0aaa9d34      1
     18 0x1c0ab03943e5ea290c4f5e6b0ebc0197f7b88601      1
     19 0x1fa83251e3773011b4ede627b1f16824c8fb8f2e      1
     20 0x2205d45163f81139fc54a7694b7d809b294b38ff      1
     21 0x22cbfccf2f80d8016d66a3189842cfec8e6127bc      1
     22 0x24a0435dd391a6962d8d99ce19f51f2b540412b5      1
     23 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     24 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     25 0x29afe421e3722443560679b97431c32d1e26e057      1
     26 0x29bd2d1bc9382ab20ee799b6a8beaf9de1a8e929      1
     27 0x2b4e8bfe0c845a181c6c0e73728a9ceb917a985c      1
     28 0x2d3e2776404cee093599d429257b6b42b14912d6      1
     29 0x313fe2f013b5ee2e2aac09b59462464db56f7680      1
     30 0x33033761bb4305e48eae6d1a8412eb5175373ecc      1
     31 0x3a5144a1c646b07e4e7543ae248745ef21c1413e      1
     32 0x3a88dcb6ed37a4b2d6d7218fdca073271095e2d3      1
     33 0x3cba9204150fe936f334fb6be47f3dfe3ce6e23f      1
     34 0x3db9ae7d5341249e15bd7d02f248a2377520d365      1
     35 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     36 0x44022a7eea59beff273e0d033d76884fa6da0822      1
     37 0x448017181f44dbdbbf1fbc48ee7734b1d0175a6d      1
     38 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     39 0x46ddfb43370dc8611e834dbdbe43e8164fbd6001      1
     40 0x478bb542f7658d635abba67edb987806dff5b83d      1
     41 0x4afd23d674e7266cc81f8cf38b332ece879da6fb      1
     42 0x4c6cdf7acac46174acd0faef67bf881bcb9230a7      1
     43 0x4cff7990f598a956d17ab11e80664bc8098d5df2      1
     44 0x4ea30897907944fbc38f4e90d2f7f939969e9347      1
     45 0x5359edabdbbcacb2aee282947025f8172c67e402      1
     46 0x53d2c392e9340b12bb62dc5b3cff07c0435d461b      1
     47 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     48 0x5c87758be1ebe8e18b82a535290b0e8525258a43      1
     49 0x5f0b54c2d75d49e06c6555085fb106d13a7a6e52      1
     50 0x640ea6c41cd910db4fb1652b4422255fa3fd4707      1
     51 0x64c2b6504c7efd7871a07bc7323d8f33e4a3eafe      1
     52 0x6506280cad8a12e969fd5af39a25a97bcc5c78a6      1
     53 0x65bd3af92157c57fd06ae0b184bcf01449461f79      1
     54 0x661fdff5fad150764d44cb3361b8000258d9b39b      1
     55 0x6b711e387693cded325c5a0c334594060034a1a8      1
     56 0x6eb7817d9c48c8aadd68ad694f9b14f3b4e3c450      1
     57 0x7034adf76d5b3fe6ca953799ba4aa05e511db56a      1
     58 0x70e49c1e07ed456f03f971b844564f9196131799      1
     59 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
     60 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     61 0x8218a2445679e38f358e42f88fe2125c98440d59      1
     62 0x86fc973fc5146132afd4421497d031a032e2de71      1
     63 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
     64 0x8e4222c444479dbba54309a511a0651d0691d7e3      1
     65 0x8e8d647440fb97f214be7b610ef916be0948375a      1
     66 0x904a819e486d4af2653eec8e424599335e2f190e      1
     67 0x91381ebe65c3ddfca0fa5bf75a4e560ecc6f5026      1
     68 0x9161da60230dad11b0284388e0dd23fc32a6baaf      1
     69 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
     70 0x961cfb2ac56ab0947798523daec1120d8ed5493f      1
     71 0x96868d278674f316a770bda6c495eebe3b89fdd5      1
     72 0x96e07daadebb4255ed0d6ad93a181218c5b52f16      1
     73 0x98175fcafcbd021769431b29bcc8fa7d7408e5fa      1
     74 0x9ace2324b888bd5a9695ea44f87f7e5d7cb9c06e      1
     75 0x9c748a6b2fd26757f6d15b82f4bf7f7aef66f4bb      1
     76 0x9fe6c2c7123134f3c861c5f002e5b4babcff70d4      1
     77 0xa29264df3cf06a72689397c094a8e32eb74863d8      1
     78 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
     79 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     80 0xa7650e468993e41f32e506e9d74be8998937ed6d      1
     81 0xa7dba3eda942455197a03f48b1dd099f9daccc1f      1
     82 0xacabfc1ef7f287525d38861069226d0e859d8ac2      1
     83 0xaf5a02182cbda9bc796e6e99f843cdc098bbbb19      1
     84 0xaff7277fb299c33ea635981369dd07af664155f3      1
     85 0xb273b47a788aa305c5c65fa6b56159a4d306d788      1
     86 0xb3cef0a2007ed9c3d3f538220e8f6bc6ba69a5b4      1
     87 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
     88 0xb9f7765eb58e4053e0f69953f52a267071f668f4      1
     89 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
     90 0xbe25e3c60b4969de23fb10930043e44f987cbc1b      1
     91 0xbe36600c3c28edf3ac2368430b8281a885feaf95      1
     92 0xbf46d2161045251cb97d0b41929bc1d36044e1a0      1
     93 0xbfb9915884de2ec1f96d999ec81c29e129035edc      1
     94 0xc0b1a85255bb74c4b5d7a17af1305f29623d0b54      1
     95 0xc645c576cf3d394322f1e9f9fa0dc07a73a3e8b2      1
     96 0xcdec862959d58d31958c15c171c4437ad9e6c711      1
     97 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
     98 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
     99 0xd7189a81961cb3cc0dd6ec6a1f90bc5f95dfd7f0      1
    100 0xda47a6b9ec624f9d8cfe1db6423a26cbebc1246d      1
    101 0xdd30bc2d51596030d0e969670dd6037e61ce43c0      1
    102 0xdf15f4225403aea61d1156c1fddd998ffc0542e8      1
    103 0xe02fa4f25099ced9f52a2898d10dc25a5e983e50      1
    104 0xe063b07ed5871962debe252fd433b7d12680d063      1
    105 0xe8ea29354a00b412e16a4d3a9642e622e99eb997      1
    106 0xe9063389a16abb73934c57e2679c2941fc0f0fd3      1
    107 0xebc5733788fd2ff26e785c9396e7410810f5c626      1
    108 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    109 0xef61dab0ea377acb75b0b2e0b9c7241d8aa6673f      1
    110 0xef82b9da9ab815b871120ff8a216ae0a7918abd4      1
    111 0xf16c9108d4d9c367459d5fb9d532e228e39ba279      1
    112 0xf613cfd07af6d011fd671f98064214ab5b2942cf      1
    113 0xf77b8f94301ca39ecfce417315f0dc1eb1fea02f      1
    114 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    115 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    116 0xfbc78f494ad61d90f02a3258e527de1321095acb      1
    117 0xfd067a330af3e78468c36cced4ea8877b3fec7d3      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 121 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00a58651879fd753ab58d639b633368859cda6e1      1
      2 0x01a8bc97c867c28eb3a1b0af5a12e761a7a6a8e3      1
      3 0x0523dbfa086483164bb773c920e6a868a5cd40a1      1
      4 0x05ee1ee157d9a1c62ee8a671ba2abeac0ae6f2d5      1
      5 0x060fb9f0d0529a80f5fb94ecad98df7df99ecb0a      1
      6 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
      7 0x0a834bbc8d2273c2416ef1cd7a4df735f60cd751      1
      8 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
      9 0x0fc5df1a76dac8a19d11d2ddcc5bbe4b79bba910      1
     10 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     11 0x11d3502407d152147ba2ca2cfe78fa8dc8e5ef70      1
     12 0x15d51e51caf5585a40cb965080098bfb68af3336      1
     13 0x17d369a86cdfa617c8d3b4bf487335dae4cfe9d4      1
     14 0x18e7df01107437323e4738222f9c8a9060616e08      1
     15 0x1a1995aece8077c0209f0064a47d7e8033b2aa4f      1
     16 0x1bec0e6266e97f0c7c42aa720959e46598dd8ce1      1
     17 0x1c028286e330a2facca02fb6bf91136c0aaa9d34      1
     18 0x1c0ab03943e5ea290c4f5e6b0ebc0197f7b88601      1
     19 0x1fa83251e3773011b4ede627b1f16824c8fb8f2e      1
     20 0x2205d45163f81139fc54a7694b7d809b294b38ff      1
     21 0x22cbfccf2f80d8016d66a3189842cfec8e6127bc      1
     22 0x24a0435dd391a6962d8d99ce19f51f2b540412b5      1
     23 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     24 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     25 0x29afe421e3722443560679b97431c32d1e26e057      1
     26 0x29bd2d1bc9382ab20ee799b6a8beaf9de1a8e929      1
     27 0x2afb4c1b3c2c4d93d71909afbf775358cb74f52b      1
     28 0x2b4e8bfe0c845a181c6c0e73728a9ceb917a985c      1
     29 0x2d3e2776404cee093599d429257b6b42b14912d6      1
     30 0x313fe2f013b5ee2e2aac09b59462464db56f7680      1
     31 0x33033761bb4305e48eae6d1a8412eb5175373ecc      1
     32 0x3a5144a1c646b07e4e7543ae248745ef21c1413e      1
     33 0x3a88dcb6ed37a4b2d6d7218fdca073271095e2d3      1
     34 0x3cba9204150fe936f334fb6be47f3dfe3ce6e23f      1
     35 0x3db9ae7d5341249e15bd7d02f248a2377520d365      1
     36 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     37 0x44022a7eea59beff273e0d033d76884fa6da0822      1
     38 0x448017181f44dbdbbf1fbc48ee7734b1d0175a6d      1
     39 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     40 0x46ddfb43370dc8611e834dbdbe43e8164fbd6001      1
     41 0x478bb542f7658d635abba67edb987806dff5b83d      1
     42 0x4949b46633b810bdd745b028062b30f6b647ec60      1
     43 0x4afd23d674e7266cc81f8cf38b332ece879da6fb      1
     44 0x4c6cdf7acac46174acd0faef67bf881bcb9230a7      1
     45 0x4cff7990f598a956d17ab11e80664bc8098d5df2      1
     46 0x4ea30897907944fbc38f4e90d2f7f939969e9347      1
     47 0x5359edabdbbcacb2aee282947025f8172c67e402      1
     48 0x53d2c392e9340b12bb62dc5b3cff07c0435d461b      1
     49 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     50 0x5c87758be1ebe8e18b82a535290b0e8525258a43      1
     51 0x5f0b54c2d75d49e06c6555085fb106d13a7a6e52      1
     52 0x640ea6c41cd910db4fb1652b4422255fa3fd4707      1
     53 0x64c2b6504c7efd7871a07bc7323d8f33e4a3eafe      1
     54 0x6506280cad8a12e969fd5af39a25a97bcc5c78a6      1
     55 0x65bd3af92157c57fd06ae0b184bcf01449461f79      1
     56 0x661fdff5fad150764d44cb3361b8000258d9b39b      1
     57 0x6b711e387693cded325c5a0c334594060034a1a8      1
     58 0x6eb7817d9c48c8aadd68ad694f9b14f3b4e3c450      1
     59 0x7034adf76d5b3fe6ca953799ba4aa05e511db56a      1
     60 0x70e49c1e07ed456f03f971b844564f9196131799      1
     61 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
     62 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     63 0x8218a2445679e38f358e42f88fe2125c98440d59      1
     64 0x86fc973fc5146132afd4421497d031a032e2de71      1
     65 0x87afc8adaff6ab99af285b3e4790b1aaac2e3461      1
     66 0x8e4222c444479dbba54309a511a0651d0691d7e3      1
     67 0x8e8d647440fb97f214be7b610ef916be0948375a      1
     68 0x904a819e486d4af2653eec8e424599335e2f190e      1
     69 0x91381ebe65c3ddfca0fa5bf75a4e560ecc6f5026      1
     70 0x9161da60230dad11b0284388e0dd23fc32a6baaf      1
     71 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
     72 0x961cfb2ac56ab0947798523daec1120d8ed5493f      1
     73 0x96868d278674f316a770bda6c495eebe3b89fdd5      1
     74 0x96e07daadebb4255ed0d6ad93a181218c5b52f16      1
     75 0x98175fcafcbd021769431b29bcc8fa7d7408e5fa      1
     76 0x9ace2324b888bd5a9695ea44f87f7e5d7cb9c06e      1
     77 0x9c748a6b2fd26757f6d15b82f4bf7f7aef66f4bb      1
     78 0x9fe6c2c7123134f3c861c5f002e5b4babcff70d4      1
     79 0xa29264df3cf06a72689397c094a8e32eb74863d8      1
     80 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
     81 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     82 0xa7650e468993e41f32e506e9d74be8998937ed6d      1
     83 0xa7dba3eda942455197a03f48b1dd099f9daccc1f      1
     84 0xacabfc1ef7f287525d38861069226d0e859d8ac2      1
     85 0xaf5a02182cbda9bc796e6e99f843cdc098bbbb19      1
     86 0xaff7277fb299c33ea635981369dd07af664155f3      1
     87 0xb273b47a788aa305c5c65fa6b56159a4d306d788      1
     88 0xb3cef0a2007ed9c3d3f538220e8f6bc6ba69a5b4      1
     89 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
     90 0xb9f7765eb58e4053e0f69953f52a267071f668f4      1
     91 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
     92 0xbe25e3c60b4969de23fb10930043e44f987cbc1b      1
     93 0xbe36600c3c28edf3ac2368430b8281a885feaf95      1
     94 0xbf46d2161045251cb97d0b41929bc1d36044e1a0      1
     95 0xbfb9915884de2ec1f96d999ec81c29e129035edc      1
     96 0xc0b1a85255bb74c4b5d7a17af1305f29623d0b54      1
     97 0xc645c576cf3d394322f1e9f9fa0dc07a73a3e8b2      1
     98 0xcdec862959d58d31958c15c171c4437ad9e6c711      1
     99 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    100 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
    101 0xd7189a81961cb3cc0dd6ec6a1f90bc5f95dfd7f0      1
    102 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
    103 0xda47a6b9ec624f9d8cfe1db6423a26cbebc1246d      1
    104 0xdd30bc2d51596030d0e969670dd6037e61ce43c0      1
    105 0xdf15f4225403aea61d1156c1fddd998ffc0542e8      1
    106 0xe02fa4f25099ced9f52a2898d10dc25a5e983e50      1
    107 0xe063b07ed5871962debe252fd433b7d12680d063      1
    108 0xe8ea29354a00b412e16a4d3a9642e622e99eb997      1
    109 0xe9063389a16abb73934c57e2679c2941fc0f0fd3      1
    110 0xebc5733788fd2ff26e785c9396e7410810f5c626      1
    111 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    112 0xef61dab0ea377acb75b0b2e0b9c7241d8aa6673f      1
    113 0xef82b9da9ab815b871120ff8a216ae0a7918abd4      1
    114 0xf16c9108d4d9c367459d5fb9d532e228e39ba279      1
    115 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    116 0xf613cfd07af6d011fd671f98064214ab5b2942cf      1
    117 0xf77b8f94301ca39ecfce417315f0dc1eb1fea02f      1
    118 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    119 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    120 0xfbc78f494ad61d90f02a3258e527de1321095acb      1
    121 0xfd067a330af3e78468c36cced4ea8877b3fec7d3      1

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
