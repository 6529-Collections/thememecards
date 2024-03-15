
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:164         Length:164         Min.   :1.000   Length:164        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.104                     
                                           3rd Qu.:1.000                     
                                           Max.   :6.000                     
         name          
     Length:164        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19396969 # https://etherscan.io/block/19396969
block_hash <- "0xfb8ad61cfdc13e2882277e41c1dba92e5bfd23bc20baeca6af00b2f504ace5d0"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5041 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","TRANSATLANTIC","HausOfCollage","whereurlsmeetirl","MineAllMine","TheWholeTownIsWaiting","CharadesofYouth","HushedMuseums","AMightyShortWalk","ValentineDayMassacre","Foundation","KnownOrigin","KnownOrigin2","NoFinerPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("WhatYouLeaveBehindEditions","NobleGalleryEditions","DegeneracyArtEditions","NG1Editions","NG2Editions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","TRANSATLANTIC","HausOfCollage","whereurlsmeetirl","MineAllMine","TheWholeTownIsWaiting","CharadesofYouth","HushedMuseums","AMightyShortWalk","ValentineDayMassacre","Foundation","KnownOrigin","KnownOrigin2","NoFinerPlace","WhatYouLeaveBehindEditions","NobleGalleryEditions","DegeneracyArtEditions","NG1Editions","NG2Editions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 60 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     3 0x0d9d376e582b82b71122814756c103cadd5a59c1      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x11a51b3af70afae1b52cf51cb38cade20c1203dc      1
     6 0x146d745139d417b8b5a1190cc73b34d7d37a9bba      1
     7 0x1619f7076866ddc852a5a0a69cbb4d6a338d6d67      1
     8 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     9 0x1f6db2c51185429a1b166321d8ae164a4c189b49      1
    10 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
    11 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
    12 0x242d0b281fd6a85b13e2035ef87d3efeb1874512      1
    13 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
    14 0x335e19b3f5dcfa2e398ee7d5ff430a1f8ccc88b2      1
    15 0x337101def3eeb6f06e071efe02216274507937bb      1
    16 0x33d7a3ad5b4168f30c051ee66afd3c2a865ed919      1
    17 0x3595a1508cb1180e8e7f50008db1109f5293efc5      1
    18 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    19 0x38e23cea342deea0744a4f68780053f347b8bfa6      1
    20 0x42ec9ab7db1ad0b3a0c77bbe77c436c58db8eb7e      1
    21 0x528eeb05a93a22a9cb1f9e507af991e264b9cf3b      1
    22 0x576a655161b5502dcf40602be1f3519a89b71658      1
    23 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
    24 0x5e93ab46e9a0b39312b5e112d4b053a768203f9f      1
    25 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    26 0x6186290b28d511bff971631c916244a9fc539cfe      1
    27 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
    28 0x7290bf7488af285e5ae095dbc5234dd2eac9b61b      1
    29 0x7535da202d79ca57299918c60c218f9b779aa14c      1
    30 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
    31 0x77037050bb27ae8f09118a0c480224d897589c65      1
    32 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    33 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    34 0x9c51532ef51ab720514b5acaf97fe6c600fbfb54      1
    35 0xa29528463c8241c1b7fe5343e93f57cac2ecb9f5      1
    36 0xab6ca2017548a170699890214bfd66583a0c1754      1
    37 0xab80cd468c733735d407374579b47cf30ca356b3      1
    38 0xabb3738f04dc2ec20f4ae4462c3d069d02ae045b      1
    39 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    40 0xb2892b27eb4fcfd6169e8e5f6bb88c1456b4f994      1
    41 0xb3007ff3c3f40bdf0126fec7c8e43c3fc50ea813      1
    42 0xb43461f2b60f63de07d80eb1a667a9b84802e8cc      1
    43 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    44 0xbd72d021d3cb334deb3151db905ee073b8eee518      1
    45 0xbddf499207d29e920c0500642567b43238b30fd3      1
    46 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    47 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    48 0xc927105490efaea1248f27e280e2296b525be157      1
    49 0xca9ba6a0974ba3a09c8fee76dfdd87ba1d6a2801      1
    50 0xcf3b555729822b93ab8f05c9124b10243cd3748f      1
    51 0xd849843971653d1d3c8d40864ab4f16080fdcbf4      1
    52 0xd8d3ccbac5df46b67aa8f3cc84a68ddc8c784b40      1
    53 0xe288a00df4b697606078876788e4d64633cd2e01      1
    54 0xe4c6c46645988bbafd8ef6b4d1b60d969cc857c3      1
    55 0xe6d6758d2fc0a7a0c476794855d035f3bcba245f      1
    56 0xe70c7cda0ee51c20c4ea4bbc34c85f5f8d404ad9      1
    57 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    58 0xf74aa398050f15c7c58750ce71cf0be5440ba47a      1
    59 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    60 0xfe10952f8af7c2c7c719988f7444dfcccba87d50      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 80 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01d71a1e369cd675a1215b9f1bd63b582f6f257a      1
     2 0x0518be2e60bd3f0da9402b02194957018865cebd      1
     3 0x09b5859c61472225eea395b1aa31e2cd976d34a3      1
     4 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     5 0x1d130d29b3906555030452f0f29cdb0b9750fd21      1
     6 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     7 0x23602ca06e977c86339ffddad74966e824ab691e      1
     8 0x279775d31fbbfa8e589631ef49c6b3d0913803e1      1
     9 0x27fb0a35ac158fe460b91b04e5087b0131352ed9      1
    10 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
    11 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
    12 0x28d4aba03b5bc52a9239ee09fc8f7edf6c844c21      1
    13 0x2dfe6174eb339036000717fe86f1450f0e43a606      1
    14 0x2e0a86c23066134b7ba0079f0419d00852048df1      1
    15 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
    16 0x367c597f1e91045607389ab4d6415bb7a7bd69ef      1
    17 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
    18 0x3d6b9ee137844e278d1ddbded29e57154f492349      1
    19 0x431060e2da89e839a5be02cee86fc1f2d03edd2e      1
    20 0x47865cbb78ac8b25b2619efe17b822a357945ddf      1
    21 0x5622cc24ddecf5888d31df4b8b12648d105472bc      1
    22 0x568a2e5713458a263da2b86cae2f6af0693799fd      1
    23 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
    24 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
    25 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
    26 0x6493182cc538c8b4d60174bde8aaa6861f862b10      1
    27 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    28 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
    29 0x707bfeed11b63d3360de40313f01092aa9b365c1      1
    30 0x7143f19d9df476227013260f015b06359918ef3f      1
    31 0x74abf717f4f069c0aaa2074fa1f80e35466079b3      1
    32 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    33 0x7a204ae5b7e4458f87c9ec5056953caf3f9aece9      1
    34 0x7a4709062134e62de720850cb941e9b006cc058e      1
    35 0x7c4731f32a2bc67dc03e14b813982f49a3867d6f      1
    36 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
    37 0x7f14a8d51a8df9a2d4c2965f4a336a1d0c173216      1
    38 0x7fc55376d5a29e0ee86c18c81bb2fc8f9f490e50      1
    39 0x827c622c85c9970b4902918bc5b843e0aa291489      1
    40 0x838c764421bc013cf61c3c281d5d4acc6d77544d      1
    41 0x8560cbe177a4b28320e22012df9c908aabe24dcf      1
    42 0x877966054785c393ef68b2c4bc45d5676915c7e2      1
    43 0x88668b63218bcfd31692e14f635d195f9f45c030      1
    44 0x8e45de3ad0f4ffa634e2d84d4995450cdb6b49f9      1
    45 0x900c0c2fd84f7385ae36421499c7b3e99c8e058a      1
    46 0x91215d71a00b0907ca67b25401172ab18d4b18f9      1
    47 0x938ca546225b55f2db2ff251e7a71ddc530f6fcb      1
    48 0x957a965e386aa1b66b61924d75ce64642b643b4e      1
    49 0xa067705a0b8482162c3244e660c77ac48e8c1fa1      1
    50 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    51 0xace1c6f4dab142925a3d628c0fa5440c4dedd815      1
    52 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    53 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    54 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    55 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
    56 0xc31da5c844177b716ecc3f78fb8b08098c222452      1
    57 0xc550716af5cfe7e6315e6d8b94d2154448b2444a      1
    58 0xc5aca861be2856d25821f3ef0317950c369044fa      1
    59 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    60 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    61 0xd821e6d6d96ecf3aed987bbaf259c761b0cd28b6      1
    62 0xd94da55433369cb75283ab5deac0158b6b7e2ca9      1
    63 0xdcd87f8c295c5cdd1dda66f11ed1a01c42a5f1a3      1
    64 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    65 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    66 0xe2bb39f857195fbd8bd0c4833a4fa28496083b29      1
    67 0xe55a329415a518e3f743cbb850f2dbc694fe4bc0      1
    68 0xe8dbd6474fb0128863f5d3204af5ef363d90adb0      1
    69 0xe96b4cc1dc97dbca6690c8eb0434181466d680e7      1
    70 0xeab3739d4e716bb07901d72dc94b4139b6968623      1
    71 0xed3d7456997d9af7d284230b5017b35d140da798      1
    72 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    73 0xeebd455c141750d4ebf27d66be57dd3c7aa3e554      1
    74 0xf5671d951c0aa6e4bd69a854fc2d15fe707ddd0e      1
    75 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    76 0xf8e90d2d2f6f67a13d0a04374e22624a80a1e918      1
    77 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    78 0xf997a1a60cf8a65185a938647bd22c6158463195      1
    79 0xfad0a4097d64e5950f04f4e96d5609a96eb6ac9f      1
    80 0xfdd9a3e4c07756c1dc31ba938fc062d45eab1668      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 140 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ff192363430a35abbf968c535b64147e88abdb      1
      2 0x01d71a1e369cd675a1215b9f1bd63b582f6f257a      1
      3 0x0518be2e60bd3f0da9402b02194957018865cebd      1
      4 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      5 0x09b5859c61472225eea395b1aa31e2cd976d34a3      1
      6 0x0d9d376e582b82b71122814756c103cadd5a59c1      1
      7 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
      8 0x11a51b3af70afae1b52cf51cb38cade20c1203dc      1
      9 0x146d745139d417b8b5a1190cc73b34d7d37a9bba      1
     10 0x1619f7076866ddc852a5a0a69cbb4d6a338d6d67      1
     11 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     12 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     13 0x1d130d29b3906555030452f0f29cdb0b9750fd21      1
     14 0x1f6db2c51185429a1b166321d8ae164a4c189b49      1
     15 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     16 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     17 0x23602ca06e977c86339ffddad74966e824ab691e      1
     18 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     19 0x242d0b281fd6a85b13e2035ef87d3efeb1874512      1
     20 0x279775d31fbbfa8e589631ef49c6b3d0913803e1      1
     21 0x27fb0a35ac158fe460b91b04e5087b0131352ed9      1
     22 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     23 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     24 0x28d4aba03b5bc52a9239ee09fc8f7edf6c844c21      1
     25 0x2dfe6174eb339036000717fe86f1450f0e43a606      1
     26 0x2e0a86c23066134b7ba0079f0419d00852048df1      1
     27 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     28 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
     29 0x335e19b3f5dcfa2e398ee7d5ff430a1f8ccc88b2      1
     30 0x337101def3eeb6f06e071efe02216274507937bb      1
     31 0x33d7a3ad5b4168f30c051ee66afd3c2a865ed919      1
     32 0x3595a1508cb1180e8e7f50008db1109f5293efc5      1
     33 0x367c597f1e91045607389ab4d6415bb7a7bd69ef      1
     34 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     35 0x38e23cea342deea0744a4f68780053f347b8bfa6      1
     36 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     37 0x3d6b9ee137844e278d1ddbded29e57154f492349      1
     38 0x42ec9ab7db1ad0b3a0c77bbe77c436c58db8eb7e      1
     39 0x431060e2da89e839a5be02cee86fc1f2d03edd2e      1
     40 0x47865cbb78ac8b25b2619efe17b822a357945ddf      1
     41 0x528eeb05a93a22a9cb1f9e507af991e264b9cf3b      1
     42 0x5622cc24ddecf5888d31df4b8b12648d105472bc      1
     43 0x568a2e5713458a263da2b86cae2f6af0693799fd      1
     44 0x576a655161b5502dcf40602be1f3519a89b71658      1
     45 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
     46 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     47 0x5e93ab46e9a0b39312b5e112d4b053a768203f9f      1
     48 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     49 0x6186290b28d511bff971631c916244a9fc539cfe      1
     50 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     51 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     52 0x6493182cc538c8b4d60174bde8aaa6861f862b10      1
     53 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
     54 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
     55 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
     56 0x707bfeed11b63d3360de40313f01092aa9b365c1      1
     57 0x7143f19d9df476227013260f015b06359918ef3f      1
     58 0x7290bf7488af285e5ae095dbc5234dd2eac9b61b      1
     59 0x74abf717f4f069c0aaa2074fa1f80e35466079b3      1
     60 0x7535da202d79ca57299918c60c218f9b779aa14c      1
     61 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     62 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     63 0x77037050bb27ae8f09118a0c480224d897589c65      1
     64 0x7a204ae5b7e4458f87c9ec5056953caf3f9aece9      1
     65 0x7a4709062134e62de720850cb941e9b006cc058e      1
     66 0x7c4731f32a2bc67dc03e14b813982f49a3867d6f      1
     67 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     68 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
     69 0x7f14a8d51a8df9a2d4c2965f4a336a1d0c173216      1
     70 0x7fc55376d5a29e0ee86c18c81bb2fc8f9f490e50      1
     71 0x827c622c85c9970b4902918bc5b843e0aa291489      1
     72 0x838c764421bc013cf61c3c281d5d4acc6d77544d      1
     73 0x8560cbe177a4b28320e22012df9c908aabe24dcf      1
     74 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
     75 0x877966054785c393ef68b2c4bc45d5676915c7e2      1
     76 0x88668b63218bcfd31692e14f635d195f9f45c030      1
     77 0x8e45de3ad0f4ffa634e2d84d4995450cdb6b49f9      1
     78 0x900c0c2fd84f7385ae36421499c7b3e99c8e058a      1
     79 0x91215d71a00b0907ca67b25401172ab18d4b18f9      1
     80 0x938ca546225b55f2db2ff251e7a71ddc530f6fcb      1
     81 0x957a965e386aa1b66b61924d75ce64642b643b4e      1
     82 0x9c51532ef51ab720514b5acaf97fe6c600fbfb54      1
     83 0xa067705a0b8482162c3244e660c77ac48e8c1fa1      1
     84 0xa29528463c8241c1b7fe5343e93f57cac2ecb9f5      1
     85 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
     86 0xab6ca2017548a170699890214bfd66583a0c1754      1
     87 0xab80cd468c733735d407374579b47cf30ca356b3      1
     88 0xabb3738f04dc2ec20f4ae4462c3d069d02ae045b      1
     89 0xace1c6f4dab142925a3d628c0fa5440c4dedd815      1
     90 0xb05b7ea3714a95857e45629def2b9c3577690208      1
     91 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
     92 0xb2892b27eb4fcfd6169e8e5f6bb88c1456b4f994      1
     93 0xb3007ff3c3f40bdf0126fec7c8e43c3fc50ea813      1
     94 0xb43461f2b60f63de07d80eb1a667a9b84802e8cc      1
     95 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
     96 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
     97 0xbd72d021d3cb334deb3151db905ee073b8eee518      1
     98 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
     99 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
    100 0xbddf499207d29e920c0500642567b43238b30fd3      1
    101 0xbfacde4341f32b5f77ad81831e128ede4b5e6073      1
    102 0xc31da5c844177b716ecc3f78fb8b08098c222452      1
    103 0xc550716af5cfe7e6315e6d8b94d2154448b2444a      1
    104 0xc5aca861be2856d25821f3ef0317950c369044fa      1
    105 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    106 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    107 0xc927105490efaea1248f27e280e2296b525be157      1
    108 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    109 0xca9ba6a0974ba3a09c8fee76dfdd87ba1d6a2801      1
    110 0xcf3b555729822b93ab8f05c9124b10243cd3748f      1
    111 0xd821e6d6d96ecf3aed987bbaf259c761b0cd28b6      1
    112 0xd849843971653d1d3c8d40864ab4f16080fdcbf4      1
    113 0xd8d3ccbac5df46b67aa8f3cc84a68ddc8c784b40      1
    114 0xd94da55433369cb75283ab5deac0158b6b7e2ca9      1
    115 0xdcd87f8c295c5cdd1dda66f11ed1a01c42a5f1a3      1
    116 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    117 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    118 0xe288a00df4b697606078876788e4d64633cd2e01      1
    119 0xe2bb39f857195fbd8bd0c4833a4fa28496083b29      1
    120 0xe4c6c46645988bbafd8ef6b4d1b60d969cc857c3      1
    121 0xe55a329415a518e3f743cbb850f2dbc694fe4bc0      1
    122 0xe6d6758d2fc0a7a0c476794855d035f3bcba245f      1
    123 0xe70c7cda0ee51c20c4ea4bbc34c85f5f8d404ad9      1
    124 0xe8dbd6474fb0128863f5d3204af5ef363d90adb0      1
    125 0xe96b4cc1dc97dbca6690c8eb0434181466d680e7      1
    126 0xeab3739d4e716bb07901d72dc94b4139b6968623      1
    127 0xed3d7456997d9af7d284230b5017b35d140da798      1
    128 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    129 0xeebd455c141750d4ebf27d66be57dd3c7aa3e554      1
    130 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    131 0xf5671d951c0aa6e4bd69a854fc2d15fe707ddd0e      1
    132 0xf74aa398050f15c7c58750ce71cf0be5440ba47a      1
    133 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    134 0xf8e90d2d2f6f67a13d0a04374e22624a80a1e918      1
    135 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    136 0xf997a1a60cf8a65185a938647bd22c6158463195      1
    137 0xfad0a4097d64e5950f04f4e96d5609a96eb6ac9f      1
    138 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    139 0xfdd9a3e4c07756c1dc31ba938fc062d45eab1668      1
    140 0xfe10952f8af7c2c7c719988f7444dfcccba87d50      1

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
