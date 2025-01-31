
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:176         Length:176         Min.   :1.000   Length:176        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.028                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:176        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21700669 # https://etherscan.io/block/21700669
block_hash <- "0xc05ce77ad4d40cc9832368d87ed7caaee8f34e052de4cdcc32fc7715b39fad83"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4951 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","MaciejDrabikNV","CityImpressions","WINDOWSPORTALS","CITIES","GUNMETAL","MusicVideos","Animations","MakersPlace","MakersPlace2"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ShibuyaStationEditions","KnownOriginEditions","KnownOriginEditions2","GUNMETALEDITIONS"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","MaciejDrabikNV","CityImpressions","WINDOWSPORTALS","CITIES","GUNMETAL","MusicVideos","Animations","MakersPlace","MakersPlace2","ShibuyaStationEditions","KnownOriginEditions","KnownOriginEditions2","GUNMETALEDITIONS"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 38 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04d7c2ee4cdbac9a0fc46d3e35e79aba5cca471d      1
     2 0x099ec486f7326542420fa873bb2db5ba3ec3cb29      1
     3 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     4 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     5 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     6 0x2eb468b1c02e6713770a0a71b95e9077a9f16651      1
     7 0x3215d92f7f2a9f135df283202bfb713c7cd572d5      1
     8 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
     9 0x364e00c07c041a1f3d74feefc5943ae6675e195f      1
    10 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    11 0x4a6cb72561623271a84490191f2429ebb611dd51      1
    12 0x4c0547bfc210267b3a07123ce048b9d715c34927      1
    13 0x565b83ccad6bf9de50b6c38a39bf5cbd401a1b6a      1
    14 0x5e7472223d5d28af97832934dcb9e5408953cefb      1
    15 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
    16 0x648a58121dc0de4436837dc585ded4fa5fba6d3e      1
    17 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    18 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    19 0x7b640407513bc16167ef3450fd6339803982e976      1
    20 0x7e6055c95c40c1b61d5b47de6f294aa644659102      1
    21 0x849a27048de274c084f42551f395823f8fa18f2e      1
    22 0x8e838ae5e4528bdb1e6a6a102ab3f31def399c82      1
    23 0x92ac0ff0eb1ddbd581446fbd507b1bf56775658e      1
    24 0x9d455affe240a25adc6cd75293ca6ab2a010ab0f      1
    25 0xa0cb9dca76bbfd9068162a834a547d5208c085ca      1
    26 0xa85e2099acfe814ebf0df040182b3f14a3b1e34a      1
    27 0xab7473da0af4658ae2879cb641d82e7652721486      1
    28 0xb38819a12d2c4b67d51e0760e5a20c9f10897e14      1
    29 0xb5a7c5bd77ee3e57a0df44de0b981721b5213daa      1
    30 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    31 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    32 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    33 0xd1289bbb6f1633bb56963bf9d51923cf9ea965e8      1
    34 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    35 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    36 0xee402489d83e2b22d496910f8c810d35a3ad7b25      1
    37 0xf2ad36b91a231efd6f3e33f8a56d8563a5885dd4      1
    38 0xffb5592fb5a51f85668ea6634c88ac1949cc063c      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 88 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
     2 0x03422275c91e065cfb65333a209820c5044d20b5      1
     3 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
     4 0x0f48669b1681d41357eac232f516b77d0c10f0f1      1
     5 0x1064aa646a7aedbd40816fc0c35e044d0244a764      1
     6 0x11873b95599234efeb361ff6c6cac038ca3b2df5      1
     7 0x1330bfa4efad9624e74790b1ef1b81b1a79b54aa      1
     8 0x1c98bba37a575a11b86cdcb3c9142e62937272ce      1
     9 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
    10 0x216ae69cf24e54e002e356ea18ad37471afc0a98      1
    11 0x21efcf0a3117f66eb9a812c39c1416f0b2d67dc9      1
    12 0x229e85226bde67462575a2202668325e6667c54d      1
    13 0x25f7986955153bca82c31b2dfeb2a9ccc97d904d      1
    14 0x25fa06431225d7acb36d59de8b7cf57b609a9edf      1
    15 0x2f27bb864a7823bd9a24a8cbc5e0da51d9dff8e4      1
    16 0x30e5b9b1f3a393bcd16f69e54c257d9e8914f6a1      1
    17 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
    18 0x32a5dc5dc6ea436d6d6d5966bece97bd8cff08f5      1
    19 0x33cecdc4b390fed171d581eba1bee54fa399b8e5      1
    20 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
    21 0x3df740ffdfaaef8b7e619182db728714caff20b4      1
    22 0x3e099af007cab8233d44782d8e6fe80fecdc321e      1
    23 0x3e64014d2d3f0c3a3b0404110cea9060b4565e61      1
    24 0x473ac44a488ffc3f8237f45e469f60b416690f3d      1
    25 0x48ae667e94363be3ab27a4a3fc0e98c657b2b107      1
    26 0x50a9be97ea8cfb0f5a92312b4b966a469bc2ab7b      1
    27 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    28 0x5119b52f7fd61cab52fc9d8f02fce8bc24159f3e      1
    29 0x51f672eb4160dd9645a3cfc369b6faf237493ffa      1
    30 0x52c2a1237b1b439422a999acb3973f234c0dd9f2      1
    31 0x559c4b03a2474b04074c3678d08fd5b8bae85028      1
    32 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    33 0x5b8660a36d3af77830a35355658bde886dad95b2      1
    34 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
    35 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
    36 0x6cb6b5f592438358704be91208a3a7420caea792      1
    37 0x6fb3ae4ecf5a42788249e95b931913a4fc3d488c      1
    38 0x72735ac3b6049a8279db58592d2f787467160203      1
    39 0x776ee1896bbcf967aeb1aac49b88e53868408c1f      1
    40 0x77d04628a093b243d9750f32893766c3d0b0449d      1
    41 0x77f55b271fdd04b01b2e623761e8abfd06defe01      1
    42 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    43 0x7dd3666633b25d8cc5c68cbfdf3907f443dae5c7      1
    44 0x7e92b902395764d4fc5c058f362b26bc75c19550      1
    45 0x83ad1c65bd64d42cb52694e8b30df7aa13ce783d      1
    46 0x8647e94eb2845e2ffe884a5c6edb7ba62a3b7fff      1
    47 0x86caeebe3d243dd6fb46833efbd6ca7f9ef08523      1
    48 0x8919014b0f6746407ce40670737bf8aab96f8124      1
    49 0x89df8ab38f6c002c42a940fa2f17cc0d6f0c2974      1
    50 0x8e4fef7a085c83791188b815c3a612c21f1abc24      1
    51 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
    52 0x8f06975b0c3cc46087134339fb22ee2d46d2106d      1
    53 0x91753df0edb7c4b2d57bbbfd3388715c72027056      1
    54 0x9576cb54b5013da91c732071feb4f8eb778a474c      1
    55 0x95eb24f50de40058b460a6ba195428f729abca32      1
    56 0x9b3580a5a1e53a03a65f323da26b15c75284f148      1
    57 0x9f533eec49dc2dbbf495f1cd687c2536d424be07      1
    58 0xa58d2bf05365cf22540a8abafbbd323e1affa117      1
    59 0xa75c93e895dc2fb3b034bf4ac59cf85c4c99ae1f      1
    60 0xa7c72e4679517b6a0f523366059fd772d0cfe79f      1
    61 0xa927d30efeec4f35f53378504080ec26e396f75d      1
    62 0xab0ca98528c1c7911cbfa8fb08abf5973f61e058      1
    63 0xac047a2e944f7a2ed4eed793b723a1911a56c1dd      1
    64 0xae8945f496fac93fafedee95fd9352af03a375fb      1
    65 0xb4ef3e6c45e39ab6b8d39bb935e57c68f4d19a0b      1
    66 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    67 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    68 0xbf82671872f56d184a132c5618edc9f150be1f22      1
    69 0xc00ae171acd39f84e66e19c7bad7bb676c1fe10c      1
    70 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    71 0xc1e549320f048d8149d5b0cb9cb268da59f1f2ad      1
    72 0xc6e4f6638927cda8362abeb9d5a2afad7c0869f8      1
    73 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    74 0xcb796a326962ff9bf94a73f3e441d68c977ca2c5      1
    75 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    76 0xd09ee980d04e2e03677aaac086e0a7b5006c180e      1
    77 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    78 0xd57721b29f2a17ab6a0635210ce05dbbecf5cbf4      1
    79 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    80 0xdcd87f8c295c5cdd1dda66f11ed1a01c42a5f1a3      1
    81 0xddeaec88e4a183f5acc7d7cfd6f69e300bb6d455      1
    82 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    83 0xe4b4bab0fad75de00e620deec5af7c137040fff1      1
    84 0xe5199e3762dd37bbf459bdbf753349287dffd678      1
    85 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    86 0xeba5c4630c35e5c85f9b6d63c50227b5ed93be1f      1
    87 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    88 0xf8797c56d27863c81240bf7f9d158519229397d5      1

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
      1 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
      2 0x03422275c91e065cfb65333a209820c5044d20b5      1
      3 0x04d7c2ee4cdbac9a0fc46d3e35e79aba5cca471d      1
      4 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
      5 0x099ec486f7326542420fa873bb2db5ba3ec3cb29      1
      6 0x0f48669b1681d41357eac232f516b77d0c10f0f1      1
      7 0x1064aa646a7aedbd40816fc0c35e044d0244a764      1
      8 0x11873b95599234efeb361ff6c6cac038ca3b2df5      1
      9 0x1330bfa4efad9624e74790b1ef1b81b1a79b54aa      1
     10 0x1c98bba37a575a11b86cdcb3c9142e62937272ce      1
     11 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     12 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     13 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     14 0x216ae69cf24e54e002e356ea18ad37471afc0a98      1
     15 0x21efcf0a3117f66eb9a812c39c1416f0b2d67dc9      1
     16 0x229e85226bde67462575a2202668325e6667c54d      1
     17 0x25f7986955153bca82c31b2dfeb2a9ccc97d904d      1
     18 0x25fa06431225d7acb36d59de8b7cf57b609a9edf      1
     19 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     20 0x2eb468b1c02e6713770a0a71b95e9077a9f16651      1
     21 0x2f27bb864a7823bd9a24a8cbc5e0da51d9dff8e4      1
     22 0x30e5b9b1f3a393bcd16f69e54c257d9e8914f6a1      1
     23 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
     24 0x3215d92f7f2a9f135df283202bfb713c7cd572d5      1
     25 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
     26 0x32a5dc5dc6ea436d6d6d5966bece97bd8cff08f5      1
     27 0x33cecdc4b390fed171d581eba1bee54fa399b8e5      1
     28 0x364e00c07c041a1f3d74feefc5943ae6675e195f      1
     29 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     30 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
     31 0x3df740ffdfaaef8b7e619182db728714caff20b4      1
     32 0x3e099af007cab8233d44782d8e6fe80fecdc321e      1
     33 0x3e64014d2d3f0c3a3b0404110cea9060b4565e61      1
     34 0x473ac44a488ffc3f8237f45e469f60b416690f3d      1
     35 0x48ae667e94363be3ab27a4a3fc0e98c657b2b107      1
     36 0x4a6cb72561623271a84490191f2429ebb611dd51      1
     37 0x4c0547bfc210267b3a07123ce048b9d715c34927      1
     38 0x50a9be97ea8cfb0f5a92312b4b966a469bc2ab7b      1
     39 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
     40 0x5119b52f7fd61cab52fc9d8f02fce8bc24159f3e      1
     41 0x51f672eb4160dd9645a3cfc369b6faf237493ffa      1
     42 0x52c2a1237b1b439422a999acb3973f234c0dd9f2      1
     43 0x559c4b03a2474b04074c3678d08fd5b8bae85028      1
     44 0x565b83ccad6bf9de50b6c38a39bf5cbd401a1b6a      1
     45 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
     46 0x5b8660a36d3af77830a35355658bde886dad95b2      1
     47 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
     48 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
     49 0x5e7472223d5d28af97832934dcb9e5408953cefb      1
     50 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     51 0x648a58121dc0de4436837dc585ded4fa5fba6d3e      1
     52 0x6cb6b5f592438358704be91208a3a7420caea792      1
     53 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     54 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
     55 0x6fb3ae4ecf5a42788249e95b931913a4fc3d488c      1
     56 0x72735ac3b6049a8279db58592d2f787467160203      1
     57 0x776ee1896bbcf967aeb1aac49b88e53868408c1f      1
     58 0x77d04628a093b243d9750f32893766c3d0b0449d      1
     59 0x77f55b271fdd04b01b2e623761e8abfd06defe01      1
     60 0x7b640407513bc16167ef3450fd6339803982e976      1
     61 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
     62 0x7dd3666633b25d8cc5c68cbfdf3907f443dae5c7      1
     63 0x7e6055c95c40c1b61d5b47de6f294aa644659102      1
     64 0x7e92b902395764d4fc5c058f362b26bc75c19550      1
     65 0x83ad1c65bd64d42cb52694e8b30df7aa13ce783d      1
     66 0x849a27048de274c084f42551f395823f8fa18f2e      1
     67 0x8647e94eb2845e2ffe884a5c6edb7ba62a3b7fff      1
     68 0x86caeebe3d243dd6fb46833efbd6ca7f9ef08523      1
     69 0x8919014b0f6746407ce40670737bf8aab96f8124      1
     70 0x89df8ab38f6c002c42a940fa2f17cc0d6f0c2974      1
     71 0x8e4fef7a085c83791188b815c3a612c21f1abc24      1
     72 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
     73 0x8e838ae5e4528bdb1e6a6a102ab3f31def399c82      1
     74 0x8f06975b0c3cc46087134339fb22ee2d46d2106d      1
     75 0x91753df0edb7c4b2d57bbbfd3388715c72027056      1
     76 0x92ac0ff0eb1ddbd581446fbd507b1bf56775658e      1
     77 0x9576cb54b5013da91c732071feb4f8eb778a474c      1
     78 0x95eb24f50de40058b460a6ba195428f729abca32      1
     79 0x9b3580a5a1e53a03a65f323da26b15c75284f148      1
     80 0x9d455affe240a25adc6cd75293ca6ab2a010ab0f      1
     81 0x9f533eec49dc2dbbf495f1cd687c2536d424be07      1
     82 0xa0cb9dca76bbfd9068162a834a547d5208c085ca      1
     83 0xa58d2bf05365cf22540a8abafbbd323e1affa117      1
     84 0xa75c93e895dc2fb3b034bf4ac59cf85c4c99ae1f      1
     85 0xa7c72e4679517b6a0f523366059fd772d0cfe79f      1
     86 0xa85e2099acfe814ebf0df040182b3f14a3b1e34a      1
     87 0xa927d30efeec4f35f53378504080ec26e396f75d      1
     88 0xab0ca98528c1c7911cbfa8fb08abf5973f61e058      1
     89 0xab7473da0af4658ae2879cb641d82e7652721486      1
     90 0xac047a2e944f7a2ed4eed793b723a1911a56c1dd      1
     91 0xae8945f496fac93fafedee95fd9352af03a375fb      1
     92 0xb38819a12d2c4b67d51e0760e5a20c9f10897e14      1
     93 0xb4ef3e6c45e39ab6b8d39bb935e57c68f4d19a0b      1
     94 0xb5a7c5bd77ee3e57a0df44de0b981721b5213daa      1
     95 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
     96 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     97 0xbf82671872f56d184a132c5618edc9f150be1f22      1
     98 0xc00ae171acd39f84e66e19c7bad7bb676c1fe10c      1
     99 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    100 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    101 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    102 0xc1e549320f048d8149d5b0cb9cb268da59f1f2ad      1
    103 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    104 0xc6e4f6638927cda8362abeb9d5a2afad7c0869f8      1
    105 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    106 0xcb796a326962ff9bf94a73f3e441d68c977ca2c5      1
    107 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    108 0xd09ee980d04e2e03677aaac086e0a7b5006c180e      1
    109 0xd1289bbb6f1633bb56963bf9d51923cf9ea965e8      1
    110 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    111 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    112 0xd57721b29f2a17ab6a0635210ce05dbbecf5cbf4      1
    113 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    114 0xdcd87f8c295c5cdd1dda66f11ed1a01c42a5f1a3      1
    115 0xddeaec88e4a183f5acc7d7cfd6f69e300bb6d455      1
    116 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    117 0xe4b4bab0fad75de00e620deec5af7c137040fff1      1
    118 0xe5199e3762dd37bbf459bdbf753349287dffd678      1
    119 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    120 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    121 0xeba5c4630c35e5c85f9b6d63c50227b5ed93be1f      1
    122 0xee402489d83e2b22d496910f8c810d35a3ad7b25      1
    123 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    124 0xf2ad36b91a231efd6f3e33f8a56d8563a5885dd4      1
    125 0xf8797c56d27863c81240bf7f9d158519229397d5      1
    126 0xffb5592fb5a51f85668ea6634c88ac1949cc063c      1

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
