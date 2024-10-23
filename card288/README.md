
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:193         Length:193         Min.   :1.000   Length:193        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.047                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:193        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20996969 # https://etherscan.io/block/20996969
block_hash <- "0xea9b1e82db889bcff21072c0ca8805f16d45cb0ddf706d0b1a7d41572c0ee012"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4769 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","Submissions","AnnAhoy11s","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("AnnAhoyEditions","AnnAhoyMadEditions","KnownOriginEditions","KnownOrigin2Editions","RaribleEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","Submissions","AnnAhoy11s","MakersPlace","AnnAhoyEditions","AnnAhoyMadEditions","KnownOriginEditions","KnownOrigin2Editions","RaribleEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 12 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     2 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
     3 0x14441ac732c0d8cf15f10043d355da11c831d828      1
     4 0x225189a1ea477a62fe4d27cf30aca8b9168441d0      1
     5 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
     6 0x3433c2753da24df566ea14c40584179e97396cf3      1
     7 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     8 0x4616c5c32aad0a2cc6163fa4b603e1832c29b9ce      1
     9 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
    10 0x762da606029d3120735aa1eec15464e265db7a3c      1
    11 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    12 0xe7c803b8282b3175f19341419eee28e6a4d04a5a      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 102 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x007c84c9523199e078be6002bf2ca8098f7bd42e      1
      2 0x074a80b54025f90046fd8589ade657987a06dfd8      1
      3 0x08ceb8bba685ee708c9c4c65576837cbe19b9dea      1
      4 0x0bf11b9122b08ff8c39b6b588a58c696c9b95d76      1
      5 0x0c5c1f28cc9433c341b30ef3129c474b52421205      1
      6 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
      7 0x0f48669b1681d41357eac232f516b77d0c10f0f1      1
      8 0x111b2086606081605c3fe501dd35f0bc8770df49      1
      9 0x144fde1ebcbd533280ac3d9fbbbf67008b3abd12      1
     10 0x17d9c051f87c4a685db7f84b54b78d61ec474090      1
     11 0x1e815a8188f1b84564577c1c998f7e6b4706b752      1
     12 0x225d6a79657fad56664b4df8bbf98aff615418da      1
     13 0x250ea0b78ba42041ccb1cba4f751b0ac743b182d      1
     14 0x27bba99469653f80efd8e3d23ba224d8db1e0851      1
     15 0x2be4aaa52893219ec124c8afc8168b7a6103811a      1
     16 0x2e0b873d26f6d90f347757ed0d041bc65e02a89f      1
     17 0x30db01087febd7b67dc486812b322b4867cb8ca7      1
     18 0x33d9cd50b4e87fb73564d3db2d45316c1810d0c0      1
     19 0x344fc9c328de743344c45a393aa8734e747d8409      1
     20 0x371d941da65350021e2d5171b4d0c4d40c02cef8      1
     21 0x3765fc1dd77de1edfaf03b4941d95983df2a5336      1
     22 0x38eff2285a0e3e2eab18c4f3b0765a833c752f1f      1
     23 0x393b70105dbfdf505862bd17c10cc8e67fe12f8b      1
     24 0x3ff1b862cf31f391d59ca29199b46cad85c2831e      1
     25 0x40596edc2e449ef48cdefc5a787f653f768369bd      1
     26 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
     27 0x4744cda32be7b3e75b9334001da9ed21789d4c0d      1
     28 0x47e255bf1ae9040151774d307c9e01d12da71d57      1
     29 0x4962aa766f1a29b3e1c5a6dd049829d593a0269d      1
     30 0x49e3cf47606a5da7b11b270a790e2112a467485f      1
     31 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     32 0x4b48670322f2e97ee4f138a4c31d14c4c1b4b5ad      1
     33 0x4edb91fc598a95a5d2361f418cb19d789a3480fd      1
     34 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
     35 0x52af787439a82f36d6ef6b0da0f0e5ecce29ff90      1
     36 0x55559be10ff6809752cfb5ab0e5d44a4a990af6e      1
     37 0x55b1eeeb65316129993e84ed2ced1472351ba1de      1
     38 0x595a729faeb20b9886f09b252549f10344f9e7ea      1
     39 0x5a483c063b6b2b4f0baafbfe799a3504dd477dae      1
     40 0x5d84cd73cc905756b44af9b565568396ba21da12      1
     41 0x5d96d8f927a7bf5f342017caf70039b9e9cfc216      1
     42 0x6006a70daaa8d9971485ebae7e3a998f6566c0f6      1
     43 0x60311403a64e47cb65e17c56da22694364740789      1
     44 0x61987699055394c65355f2797d3e4e589f7fabf4      1
     45 0x63af0b9a6597dbbbfae45938e7a6929660ea0be1      1
     46 0x68bb432a5776c90c5cbd0103c0fc652ba2f8c31f      1
     47 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     48 0x6fd155b9d52f80e8a73a8a2537268602978486e2      1
     49 0x704c978d50590b21e4c2f43eb4da24fe61fcd707      1
     50 0x72426ce863dbf454fe945fc3fd9087d4a94113a8      1
     51 0x7442ae0096b7065d6de11f204f418b7ba1859317      1
     52 0x74550e8a0dc9e16ff5d6b251a23c7d9cde912ba3      1
     53 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
     54 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     55 0x7d461bb03e204ca5dd3eefeae6cae9febd823054      1
     56 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
     57 0x7d85806c589d9e1301898932a32108f15c6daf14      1
     58 0x7dec37c03ea5ca2c47ad2509be6abaf8c63cdb39      1
     59 0x80d7a0a61bf3c8982b74daf4a51966576b2ed5c6      1
     60 0x8a2c9f1ecdab5d32811f7daf495e89b4058fb233      1
     61 0x8d1efde271a28a440af00153a182e37edbd231e8      1
     62 0x93e35173e398fb39150892a35ceb7fdd8fabf52e      1
     63 0x940a37b11ac6470ce12534e95218c06df5bd7244      1
     64 0x94ede2010554cca011c8bf52dca82dbcf2f8f2ee      1
     65 0x979975167e62e4dc91271da00612b18eabe5b26a      1
     66 0x9a1a94e850fa0348dc64c396dfaf8209289a94f0      1
     67 0x9f0574a5f28779a1a682cfe4bbd090ee058d07aa      1
     68 0x9f7064dfe6ca5856b3afd7a3b3974686a35bdab5      1
     69 0xa2a469c0138dd55d3eecf8204e10ae03551d8ea5      1
     70 0xa2b3b5c4ab5f3d3afdd2f73352a852a6e8c4df65      1
     71 0xa2de2e89c4df5b3c5e329a15e64e3071d44060ad      1
     72 0xa4ad045d62a493f0ed883b413866448afb13087c      1
     73 0xa55fcd8eeef4cac8cc774e9d4b92d2367ed47b87      1
     74 0xa7e6b17f94831d4b60c1d7ff4ee4acc2404dfa05      1
     75 0xa8e376248fb85dd9680fdbeecc3ec72e20c37cac      1
     76 0xa98425e8df39441e664c88001397402472506fad      1
     77 0xaad11a37b820f7981b66a41aaa03c076867f1705      1
     78 0xab3627317c43d394eb171170f818153b976d28a3      1
     79 0xad9f4655afbb676cf93b324c6d0852498a4f48f0      1
     80 0xafe65fa1f7402cb69f9f6ec5d506e43685bb75f6      1
     81 0xb4cd6077d45650e43654a047c93a678affb6ea0b      1
     82 0xb56ae8a727cf38f1f4716aeda6749d2af340d8f4      1
     83 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     84 0xc1b5bcbc94e6127ac3ee4054d0664e4f6afe45d3      1
     85 0xc4e60829d772ca5f566e45211becbf5f4cc763b5      1
     86 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
     87 0xc721ee4cb7e6405831fcdd044a7ef78882a1ca7c      1
     88 0xc9d25b9a3496c776688833d6ccfe507ef4f41645      1
     89 0xcc0dd5427fbf83b024966f88785988358a65a1a7      1
     90 0xcd0b3ad54ff89f5aaf3497874b9f18a91b0889ae      1
     91 0xcf2872074b74301ddbac1b9547bd4ad942791f2a      1
     92 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
     93 0xd3502a41edecc95cc07a339dffc92e764e45090c      1
     94 0xd66ce70a562bc30b203a33eaaa1163dbc303f2ad      1
     95 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
     96 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
     97 0xeb42b12a965cfc16878a966c635e04f15146c665      1
     98 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
     99 0xee4243eea50d8840f459da4fada53679aec1c702      1
    100 0xf214620443981a3606e9a26636a3dc241424760b      1
    101 0xf2d038b9bd30c55a9405975dcdf476d57b16b3bd      1
    102 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 114 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x007c84c9523199e078be6002bf2ca8098f7bd42e      1
      2 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      3 0x074a80b54025f90046fd8589ade657987a06dfd8      1
      4 0x08ceb8bba685ee708c9c4c65576837cbe19b9dea      1
      5 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
      6 0x0bf11b9122b08ff8c39b6b588a58c696c9b95d76      1
      7 0x0c5c1f28cc9433c341b30ef3129c474b52421205      1
      8 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
      9 0x0f48669b1681d41357eac232f516b77d0c10f0f1      1
     10 0x111b2086606081605c3fe501dd35f0bc8770df49      1
     11 0x14441ac732c0d8cf15f10043d355da11c831d828      1
     12 0x144fde1ebcbd533280ac3d9fbbbf67008b3abd12      1
     13 0x17d9c051f87c4a685db7f84b54b78d61ec474090      1
     14 0x1e815a8188f1b84564577c1c998f7e6b4706b752      1
     15 0x225189a1ea477a62fe4d27cf30aca8b9168441d0      1
     16 0x225d6a79657fad56664b4df8bbf98aff615418da      1
     17 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
     18 0x250ea0b78ba42041ccb1cba4f751b0ac743b182d      1
     19 0x27bba99469653f80efd8e3d23ba224d8db1e0851      1
     20 0x2be4aaa52893219ec124c8afc8168b7a6103811a      1
     21 0x2e0b873d26f6d90f347757ed0d041bc65e02a89f      1
     22 0x30db01087febd7b67dc486812b322b4867cb8ca7      1
     23 0x33d9cd50b4e87fb73564d3db2d45316c1810d0c0      1
     24 0x3433c2753da24df566ea14c40584179e97396cf3      1
     25 0x344fc9c328de743344c45a393aa8734e747d8409      1
     26 0x371d941da65350021e2d5171b4d0c4d40c02cef8      1
     27 0x3765fc1dd77de1edfaf03b4941d95983df2a5336      1
     28 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     29 0x38eff2285a0e3e2eab18c4f3b0765a833c752f1f      1
     30 0x393b70105dbfdf505862bd17c10cc8e67fe12f8b      1
     31 0x3ff1b862cf31f391d59ca29199b46cad85c2831e      1
     32 0x40596edc2e449ef48cdefc5a787f653f768369bd      1
     33 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
     34 0x4616c5c32aad0a2cc6163fa4b603e1832c29b9ce      1
     35 0x4744cda32be7b3e75b9334001da9ed21789d4c0d      1
     36 0x47e255bf1ae9040151774d307c9e01d12da71d57      1
     37 0x4962aa766f1a29b3e1c5a6dd049829d593a0269d      1
     38 0x49e3cf47606a5da7b11b270a790e2112a467485f      1
     39 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     40 0x4b48670322f2e97ee4f138a4c31d14c4c1b4b5ad      1
     41 0x4edb91fc598a95a5d2361f418cb19d789a3480fd      1
     42 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
     43 0x52af787439a82f36d6ef6b0da0f0e5ecce29ff90      1
     44 0x55559be10ff6809752cfb5ab0e5d44a4a990af6e      1
     45 0x55b1eeeb65316129993e84ed2ced1472351ba1de      1
     46 0x595a729faeb20b9886f09b252549f10344f9e7ea      1
     47 0x5a483c063b6b2b4f0baafbfe799a3504dd477dae      1
     48 0x5d84cd73cc905756b44af9b565568396ba21da12      1
     49 0x5d96d8f927a7bf5f342017caf70039b9e9cfc216      1
     50 0x6006a70daaa8d9971485ebae7e3a998f6566c0f6      1
     51 0x60311403a64e47cb65e17c56da22694364740789      1
     52 0x61987699055394c65355f2797d3e4e589f7fabf4      1
     53 0x63af0b9a6597dbbbfae45938e7a6929660ea0be1      1
     54 0x68bb432a5776c90c5cbd0103c0fc652ba2f8c31f      1
     55 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     56 0x6fd155b9d52f80e8a73a8a2537268602978486e2      1
     57 0x704c978d50590b21e4c2f43eb4da24fe61fcd707      1
     58 0x72426ce863dbf454fe945fc3fd9087d4a94113a8      1
     59 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
     60 0x7442ae0096b7065d6de11f204f418b7ba1859317      1
     61 0x74550e8a0dc9e16ff5d6b251a23c7d9cde912ba3      1
     62 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
     63 0x762da606029d3120735aa1eec15464e265db7a3c      1
     64 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     65 0x7d461bb03e204ca5dd3eefeae6cae9febd823054      1
     66 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
     67 0x7d85806c589d9e1301898932a32108f15c6daf14      1
     68 0x7dec37c03ea5ca2c47ad2509be6abaf8c63cdb39      1
     69 0x80d7a0a61bf3c8982b74daf4a51966576b2ed5c6      1
     70 0x8a2c9f1ecdab5d32811f7daf495e89b4058fb233      1
     71 0x8d1efde271a28a440af00153a182e37edbd231e8      1
     72 0x93e35173e398fb39150892a35ceb7fdd8fabf52e      1
     73 0x940a37b11ac6470ce12534e95218c06df5bd7244      1
     74 0x94ede2010554cca011c8bf52dca82dbcf2f8f2ee      1
     75 0x979975167e62e4dc91271da00612b18eabe5b26a      1
     76 0x9a1a94e850fa0348dc64c396dfaf8209289a94f0      1
     77 0x9f0574a5f28779a1a682cfe4bbd090ee058d07aa      1
     78 0x9f7064dfe6ca5856b3afd7a3b3974686a35bdab5      1
     79 0xa2a469c0138dd55d3eecf8204e10ae03551d8ea5      1
     80 0xa2b3b5c4ab5f3d3afdd2f73352a852a6e8c4df65      1
     81 0xa2de2e89c4df5b3c5e329a15e64e3071d44060ad      1
     82 0xa4ad045d62a493f0ed883b413866448afb13087c      1
     83 0xa55fcd8eeef4cac8cc774e9d4b92d2367ed47b87      1
     84 0xa7e6b17f94831d4b60c1d7ff4ee4acc2404dfa05      1
     85 0xa8e376248fb85dd9680fdbeecc3ec72e20c37cac      1
     86 0xa98425e8df39441e664c88001397402472506fad      1
     87 0xaad11a37b820f7981b66a41aaa03c076867f1705      1
     88 0xab3627317c43d394eb171170f818153b976d28a3      1
     89 0xad9f4655afbb676cf93b324c6d0852498a4f48f0      1
     90 0xafe65fa1f7402cb69f9f6ec5d506e43685bb75f6      1
     91 0xb4cd6077d45650e43654a047c93a678affb6ea0b      1
     92 0xb56ae8a727cf38f1f4716aeda6749d2af340d8f4      1
     93 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     94 0xc1b5bcbc94e6127ac3ee4054d0664e4f6afe45d3      1
     95 0xc4e60829d772ca5f566e45211becbf5f4cc763b5      1
     96 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
     97 0xc721ee4cb7e6405831fcdd044a7ef78882a1ca7c      1
     98 0xc9d25b9a3496c776688833d6ccfe507ef4f41645      1
     99 0xcc0dd5427fbf83b024966f88785988358a65a1a7      1
    100 0xcd0b3ad54ff89f5aaf3497874b9f18a91b0889ae      1
    101 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    102 0xcf2872074b74301ddbac1b9547bd4ad942791f2a      1
    103 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    104 0xd3502a41edecc95cc07a339dffc92e764e45090c      1
    105 0xd66ce70a562bc30b203a33eaaa1163dbc303f2ad      1
    106 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    107 0xe7c803b8282b3175f19341419eee28e6a4d04a5a      1
    108 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
    109 0xeb42b12a965cfc16878a966c635e04f15146c665      1
    110 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    111 0xee4243eea50d8840f459da4fada53679aec1c702      1
    112 0xf214620443981a3606e9a26636a3dc241424760b      1
    113 0xf2d038b9bd30c55a9405975dcdf476d57b16b3bd      1
    114 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1

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
