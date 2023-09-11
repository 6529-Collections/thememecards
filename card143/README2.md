
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:12035       Length:12035       Min.   :1   Length:12035      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:12035      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18104469 # https://etherscan.io/block/18104469
block_hash <- "0x5be4c09f7d9fec689f653e7bb48f597b524a7278b529c59c80cc747b4104988d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4610 

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

allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
```

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random300Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00acf1f79ff43b1ff5b4ff28b537cbf27445869f      1
      2 0x00baeaa84510584a6e4624964b75321ddb6766cc      1
      3 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
      4 0x022d46788c21bc8d933c745387e107c9fd9aef95      1
      5 0x027cae2ed1a23350a751452e907b4120330f9762      1
      6 0x030571c35b84e01613383d631c10cc8afaf83977      1
      7 0x0439ebe6168c80ff8caf438dc902090b77326555      1
      8 0x047cd96372ddff0e38a836d4f6758cc1cf2954e3      1
      9 0x07fbc53d2ffba8a9cfc0aee2ae69fd691fd6d9ce      1
     10 0x084c2c02a1448dc79692d8908648cca6d5e0b1d1      1
     11 0x08d4c631940a224181e70b6c81a1fee6dbf3251e      1
     12 0x08f189a5983980cfb5b613065ad5fbf39c62fc59      1
     13 0x09e6267d0cb691fe647da5029d28ab0fd62d9325      1
     14 0x0ac619a43eb536b8522c6e39403a47a495856c5e      1
     15 0x0b6449dcadc6f84cb8b0ad76eb9615f3a9adf37d      1
     16 0x0fafa30d377704eada7a9c83f374f409b3b46046      1
     17 0x1022245eb550f9778209ca4514dcb693b45498be      1
     18 0x110886b13e4a932396c0e9d53bf887770ca16784      1
     19 0x12d61dc3d789a1c8c8a3b88d0f2e980d2d1c5cd8      1
     20 0x13f9a182272f3ce3193fbda5459a8919d27624c6      1
     21 0x15698a313fce62baed7a64dba44849977d98e8c0      1
     22 0x164a4787cb79ee24998d3a5a4fc01cec7a3ef524      1
     23 0x16ae62a3c2b3ff7c849a9dab8fd9dd6908b9b791      1
     24 0x16bf2b9490348b21bea767ff4a3d3c82e3059f12      1
     25 0x16e9cfe34541d8601da1fab1707fe2b1b867887f      1
     26 0x17106b7ee3c4d1ad690b9caf2b2a2a6e1bde49d0      1
     27 0x1854a7a0b4f9466dbeeb3279e247d4d0b0a06e3b      1
     28 0x187a22f8aaba20adce50ef5e3998b23826e6ae22      1
     29 0x18ba19ef5c0e060c7c0bb1e622c78d6fa493e2f7      1
     30 0x18e7c894cc22dd26c60c0d579ac05a040ddefee0      1
     31 0x1af42e861037f45e1afde49917d545cda5171a72      1
     32 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
     33 0x1d41071761c941e854838583d1be87a2449dd036      1
     34 0x1de387d8d4d7000910761d7030f58568d207b56d      1
     35 0x1e2f87542ba49c77d71cd28d9d8e70f0b642288a      1
     36 0x1eb44b180681e39c3a362090e43cd6e8ce16588f      1
     37 0x1f623ee324ca178941545f01bfccfea38cda2072      1
     38 0x20d66886333375814919235674e68c7df7504659      1
     39 0x222fbcd2b08137638dcdcd1d8819cfd8cb06f0b8      1
     40 0x223e42994eb3c7b4953c86973892ccf0bab4374a      1
     41 0x23a9984e767d321c49f0564e59edda2befbcf7dc      1
     42 0x25120c6122ee525b87e7b58cacf8a3f74dffc922      1
     43 0x25f1a3ce7d9dea98d51c18b95758d417a9a641af      1
     44 0x28a8e6441f7d9fd324fb13b477748b9c30d34f0a      1
     45 0x28b8906d06784d25557b6b6d9705455b265809b3      1
     46 0x296132e19ae5737400ca45bcc8c8555982d41d53      1
     47 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     48 0x2ac3b47e7bc9d42822c1db3e6948c1a47051e805      1
     49 0x2ae11ec93845b283579164436505efd1fa6b1ce9      1
     50 0x2b3f8bec92fa0d34637f42ed865917aded146fef      1
     51 0x2b4b9e8a4527d5395bdf70d031e8487572cfa105      1
     52 0x2c49a87b71556887d6cbf88505234e8292c55173      1
     53 0x2c597519f3301a4fabb1223f9dfbcb40721f0b74      1
     54 0x2cabcfdfb87df7e03d5ea9491c1362d8bfc596b5      1
     55 0x2d15de707bb3cab89fd365f8cfa544964bf4a3e2      1
     56 0x2e7d0d8e1912eae8243c557bb66196fd1c8577b5      1
     57 0x2f68a3bab7172b3d1b5b23e71d44e220665953d7      1
     58 0x2f77ca1f5339bcbdd99d466bea714d3d87f3a422      1
     59 0x31e7bb24ac5e3684345514206d1f2fcdbfed557e      1
     60 0x3299b96d78aada4191e4aad22e585fb613f3b420      1
     61 0x33a1a5fb1b4a3652c446b50bf4d15204c4d1be24      1
     62 0x34d49e69b180823aa7b6d3ba0d1e747212d54c6c      1
     63 0x3618190bd2334fa48404bfe42179fbdb8c65b8d3      1
     64 0x372ced7af27e31828db5ad1d1b09417c14430fb2      1
     65 0x3753d9da9bef05c0f51e5bb4dfa390cac639073f      1
     66 0x3804107dbabaf63d3b8e2b11fe43fa3caa811fe9      1
     67 0x3861dd49aa50ca22c61bc0e3bb0f3d597cc15232      1
     68 0x3932fb6453fe830eef57b77f483837ddd3fe3f3a      1
     69 0x39b0b2bb9c5dc3c63a715c16f9115d456a212780      1
     70 0x39dfeed68d0c848f3927cf8d01663ea639ec5b3f      1
     71 0x3a817436d6900e9e45c517144ebbd0e1f99745f5      1
     72 0x3a89cd7d1cf3eb657f70954f0011548675c7132e      1
     73 0x3c2550ddb9294c21d209999bdaa15f8249d0c161      1
     74 0x3e437dc1f56803ac2d9d79fadb8dc561fde3f233      1
     75 0x3f1e2953ec44a6d73ab54e5c6a07b099e7dac7d2      1
     76 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     77 0x40b471854fe343ac40e1c130ea7c0382a8d04c05      1
     78 0x40d500be11e713a66f89b4cebc6d8d11e3a0f2a7      1
     79 0x40edb3a1d959c927b5113b5a4f803443f552f006      1
     80 0x41aec789f14e50b2628fc2fbcc29f909d9a97236      1
     81 0x43d3026ab05f1fddfb5621b4f5b35908f9b43890      1
     82 0x4435bc1299b1398fd58f129854de26d60ead6ab4      1
     83 0x450a6049e00c72c8205424a1f3d752ad06382fb9      1
     84 0x46a2524628486bc88a60d55af4dcd598a5ced4cb      1
     85 0x46e46a9617d0d3c8db757e7c21f27d3aa9c0f179      1
     86 0x47029dc4f3922706bf670d335c45550cff4f6a35      1
     87 0x473f998299294733187a36ce31257826eea39495      1
     88 0x475d8fbc0c4115f8fa8c86dda8814b1401d77141      1
     89 0x482b9ea86052bcf19ec00a13188abd9522398911      1
     90 0x48488c36c8643f6eeb13f6bcd5be8612918c6590      1
     91 0x4ba7a675b583cd228fb6ff23232c8dd41cd715de      1
     92 0x4c14c82fb34796ffd8a24b0ce6827ee8bc640f1e      1
     93 0x4c7cbba31f8abd69a3a931ea86e721fb9b9891f8      1
     94 0x4d2cd3e1d994ab457197efef5b8057783c2f7d32      1
     95 0x4dc574c1a6ccd3d85dcecb6bd8b600f2fe8a2643      1
     96 0x4dff50cfe5a2d72971f944132de19d2a64e978a8      1
     97 0x4f81f35b7dab9e7521c6d8397f3aa52533a62d63      1
     98 0x5118e6d7c4653c315a0c013be6a5bd3b5f5d6c3a      1
     99 0x511ef2d9d1b08b0bda6770448aca66df803998d4      1
    100 0x51fb095c8d7ff783cf884d011834192c46bd9398      1
    101 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    102 0x52edaadb86ca9a7b9647a6ed257509c3e203bd66      1
    103 0x53b9fb382a61b81814ea765f2ae4263c79b815d4      1
    104 0x54f6a34678d5b78205b024dc5db6222c3bee9e6d      1
    105 0x55205fbabda8036697dd087135440dd92df3ec36      1
    106 0x55526d47397a28bb196e516599f65e0a27eff33a      1
    107 0x555a5083c82ca88cd4b4c2d0941495c9198ce6b8      1
    108 0x55a0730a844727bd694fd8f938a456158be0f8a6      1
    109 0x56b6673c3bc30fbd588ea60bd32228ac1947387d      1
    110 0x56d3e41d87996b59842e20d638280829b5e446e9      1
    111 0x56efec6d471fbc7621db220ca52b735173cd2bc4      1
    112 0x577ec302c94f57d09b7fa1006e6165554d576f04      1
    113 0x57a15518b0ab5309d6459dc32a8acd1e89e4f333      1
    114 0x587d5b38d1ccfe08fa59230c1d5bbdd8e66e4ab1      1
    115 0x590f4ca9b70860d1b89be8a7e69f22e59f6dcf6f      1
    116 0x59e891b2368acadde5bab30c8eb1728a1e49c4f3      1
    117 0x5bc926e531431b5a1a0f26e2dd4a7aa8f322b1ce      1
    118 0x5c0f520a980197543d847e6719457c6990eed1bb      1
    119 0x5cccccbd3c8083e3ae3d37df50d1df7f7f944236      1
    120 0x5d15989394195207534a9ecbf582d712a2d2ebe8      1
    121 0x5d1792dbadd764f293cc12da86a40bd1aea4bc57      1
    122 0x5d181a27b5a2f444ddf4b89d65c88214d7413ada      1
    123 0x5d904c98df910e30516a3520d7ef4b601b73bf96      1
    124 0x5f5b5a6b4661c2a4280984b3921ffd3d58bd42f1      1
    125 0x5f603dfd9f82b7cfea8a03ad7c207a3fb3e5a8a5      1
    126 0x607bb997dd7eef475d694e9391a082941a490792      1
    127 0x60d4d538ff67e6bd2bd14d3aa724014ef01abb93      1
    128 0x60dd7313afa4a59a78cf885be5de99c594d6e49d      1
    129 0x61f9dc3a073f06a5c70cd48c7a85ea4203087c9d      1
    130 0x626d75920e4ecf153fa8efd61d31dd26d5cbf932      1
    131 0x629fd2b73c8e01888597e3d2344ec6098575af13      1
    132 0x656e08efe4fb1433781ffdca8411458ffd8575c1      1
    133 0x65a1e9c3548e6847967380aeee69403f6b58ac88      1
    134 0x65a5c6ba0f55c1c0e333bfe0db783aa07e8310ff      1
    135 0x669ecdde4a56480c48bc5a7f243cd94072bd5f94      1
    136 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
    137 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    138 0x67b8decf8c010d070126b96297c322b8f7b3491f      1
    139 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
    140 0x6dc83272ac6e09044cbab1e8f279e908b25d0c4b      1
    141 0x6fb4eea73ecb50d1d7a060c957967d0a7d379a66      1
    142 0x7051beebdc1fe5a678a8504370a15f9491b30688      1
    143 0x72baea3565cbda3ce3fbf7a305262964e918185d      1
    144 0x72c293eefdb532bd448e876174bd54161f984096      1
    145 0x72d037f63a6c782f85c059fc917a12e71b2e0c73      1
    146 0x72d2731a30b0bf4abfbd2f98391c5157645b36a5      1
    147 0x73e4a2b60cf48e8baf2b777e175a5b1e4d0c2d8f      1
    148 0x74a228c2e1f7d8f4b0a336bc024021853da8a696      1
    149 0x75775181080b3684cc3be770ba070d1ecc1ec50d      1
    150 0x76e9037854329cdb14ee78b07337bd79114f6016      1
    151 0x76f499ad599ab376471a62768f1f30a764f2e038      1
    152 0x786f0558ba618f56478b01d163248246ca94b29c      1
    153 0x78941100d90c5b66b05b52e720549e8d33e89cdf      1
    154 0x78fb3d569650ea743fb7876312cb5ff7505dd602      1
    155 0x793aba8ff4b3caadba0aa143326ae8fad5b12412      1
    156 0x79d18826ae673c828f0beb90eaa98f97eaa616b1      1
    157 0x7aefd0856861060603fce2910a95d576f6f9fbe0      1
    158 0x7bb5cc37a81623d6556d64c5d89333d3aee2a39b      1
    159 0x7bc7620d0d84ef434cfa527e0716ae5716e24b02      1
    160 0x7d4d3e72d540474e24bfdd19644e8df14581664f      1
    161 0x7fd7e17bb15ceaf850376453dea842386f07aa12      1
    162 0x806918f320b1e7714e01b2d37902e54486199e5f      1
    163 0x80b946cf5052620cd1e227ee501aa8f1cb896df3      1
    164 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    165 0x81c2eee2902eda6db28a448d8a814f221718ba2d      1
    166 0x827f541d27beb06eb0441a4ac7d4844cbbe95c0e      1
    167 0x8436e9a3aed9f8d595684edb1fa7ebd6ea6a2cfa      1
    168 0x8492d93fbd8cb147d2496cd75a2b0e30a81b8d4a      1
    169 0x85c0c09570c955f6e07522e9ad6041716cbbb7fe      1
    170 0x85de9bedc3686f8c97890980b57ce57ca2d20274      1
    171 0x87193fe62b63b811bbab15842c413b3bae94a6a2      1
    172 0x8788a09c09fe22fae8edea716fe770563d2da37f      1
    173 0x8b7d031fb0d8aab5a985583c6c56c4fffab27ee7      1
    174 0x8c3996617922c3ca288958c233141d2672b6c4cf      1
    175 0x8c48b40dba656187896147089545439e4ff4a01c      1
    176 0x8d52872f8c0635f8ba68f5c8c2162ee463ce6fdf      1
    177 0x8d9197335361537fb0cf95d256a9a0aebe6030c7      1
    178 0x8e2a6185d7c37aaab6375f9737ba0a764cde36e0      1
    179 0x92fb3ac03ebb82da2a3e7ea01e6883b7cd5dc626      1
    180 0x934fd1f764eae2efd9222e9ec683a91db1adecf8      1
    181 0x95f9f2fc223110ae09a8140c9a60b848434a652b      1
    182 0x968a9e8b72205c76c6b2a0701e58bc7165f7bba6      1
    183 0x97294b51bf128e6988c7747e0696ed7f7cfee993      1
    184 0x975fc3062c790429e40b1085728b3938d5bcef72      1
    185 0x98bd03c7202cae140810febce681563b17bf91fe      1
    186 0x98efd2126b1b76a5f426715ffbdd286c3f7e740d      1
    187 0x994b2a94886124b1575f97955d61f78a7d66f12b      1
    188 0x99768daa30519851f224d0e4e47546bd1733c219      1
    189 0x9a66910f8a09ca9b10436b802c5b96f8e63c1af3      1
    190 0x9af5c0a2eef61a4f6202532bab6a6485fbb6a287      1
    191 0x9e175101522a47aa9f79669994419f52f5d629f1      1
    192 0x9f49288c4658660c82dd98019f600a3d35969fd0      1
    193 0xa12f331e927de32c3516982a21407e676dbd5e92      1
    194 0xa159a996ed582d183d98fed1989b2f0a95a0cf63      1
    195 0xa266b9edf05b39a6d2bd9efbe1d5445192bda55c      1
    196 0xa2b4c342de8fd84bc635ee0717f61830ce40980b      1
    197 0xa2cdf058cdca7620e65cd39e432e612a1315bbf4      1
    198 0xa2d51b5832d7f5972e82ad985c408134d3473817      1
    199 0xa4322339b2726a561abf9b911f76ab00429e7a4a      1
    200 0xa5ab762aee588cb38d00ff162bba990451de71f3      1
    201 0xa76b79f978cd069d77d68cd3f82845fd7da80bca      1
    202 0xa7ad4296d9f41434d1982a4c4cd98df88121858f      1
    203 0xa8ba075e79241d0771fc4237174c1a8d67edf7d2      1
    204 0xa976fb8ed77bc1016a06074ff5e3e4a5fa6161e0      1
    205 0xa98200213c5cc919f617947d9f4a5281792d31f8      1
    206 0xaaa9aca0d2aff48aab572de4d24088a80f2ae452      1
    207 0xac4dcebd7a69144a9593f3459bfcbc2db4ebf09d      1
    208 0xac6fdcad572b38e9a8f272f10c98e5842b91da4c      1
    209 0xad2d6387a191586ae552756ba3506747ee877fbb      1
    210 0xad9f4655afbb676cf93b324c6d0852498a4f48f0      1
    211 0xae1788699d4c34808d1d56479c618f2c024a5b52      1
    212 0xae5ae13a008b662336ff3674d920819102ae4256      1
    213 0xae91cb00c413a8d6089ba0bc8bf66fba47a912ea      1
    214 0xaf21937a1c01aeeeb708bdd964042aa40e204f41      1
    215 0xb01ec69454206dea9832eb6868733e406c91ba05      1
    216 0xb0aac0747f9aec9a1121f62efbc27ffb7ac63c9c      1
    217 0xb1308c5228864109cb519796a7ae4dbea9969592      1
    218 0xb175855d114f6560105e0d7c608785466879a27d      1
    219 0xb1904abbff9c65e538b7b4cf9e876c801a2bef23      1
    220 0xb1f7d890b786e73a01ad4cce1693f1db3d3c9a99      1
    221 0xb5e7594ec5c93498123571dbac1e7c1699afb768      1
    222 0xb667344a59c3b1e2ca997dd5fa981466531eea88      1
    223 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    224 0xb87f629af14aa5dd414cb3ca2c07009175c8a0be      1
    225 0xbae03fcba11825f680927750a33532e01fd60c3e      1
    226 0xbbc56aa6b885e28d4822677f6c25d76cc2856c2d      1
    227 0xbd0f5d4be49f83fc26925d454533da2e2504da6a      1
    228 0xbd79b7e0cae4500eb02e6f212ddf2681ac8fb6d4      1
    229 0xbe527aa383c9979d0133d27b8ea1c43b694d6f9a      1
    230 0xbf98fb88344f74a9a94629d6ae9988055d1ceb76      1
    231 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
    232 0xc26b13bd421873ca9aad82dc87b5880d636b881a      1
    233 0xc2fa34a6fcb085f5e0295f233fae7fc90fbafe85      1
    234 0xc3b8bbd76c78a0dfaf47b4454472db35cebd1a24      1
    235 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    236 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    237 0xc6165f99edc566743a0528b1d8bfc6a038e8e4fc      1
    238 0xc70b41c06cd32245effd5fc34881e33022347cf1      1
    239 0xc8c90c83fd08d7e66703982de7a6177732240ca0      1
    240 0xc9dbad3feaa6603b4ea53b89e1a737ad2cdd54d6      1
    241 0xcb69c5838afd73317abe43ea23574ddf7a6e51b7      1
    242 0xcbe2dc51be3b5b03a47e9eed7005448e9fe40791      1
    243 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    244 0xcc95fb46ce61d885ff396288277aea4c13950e09      1
    245 0xcde7970b78ee805ff4b0c32a7d3f23e40bcc9554      1
    246 0xcf01b135efce6bced0e2b8ec55eef6380dfef07d      1
    247 0xcf7e084d9068634b94d18cbe979743a591f84e3c      1
    248 0xd35c49a60f9d5fec3ea2c43616ca0e2dc84a7af0      1
    249 0xd430fc849c60107201ea4f569108716a90ab65e2      1
    250 0xd52f40f2d422fad4a1cb9666e58f080d69f7de39      1
    251 0xd5a0ba47d5217418992d6887b3bf044961cdec35      1
    252 0xd62469de1d8da954cd5a054961f1393b4f871c9e      1
    253 0xd62a862da027716a0f72ea8be3ee78d2ee14b9a0      1
    254 0xd6b3b29d60b7308bb14c46b1f2f6200b88c743f2      1
    255 0xd6cc8bf1a2bdf94be558a40b2a665a46c94211b6      1
    256 0xda5f988e5fdc51f1a5d72cd6924671709615af16      1
    257 0xdcbf46cbb479ad18861b488acb13af096ab88368      1
    258 0xdcfe06271a89d454ff3302bab78d564ad6952607      1
    259 0xdd3adf8a817421dc7a7434c9cbb148601396ac44      1
    260 0xdd4dd1a8fd014861a7a705f31f0dbb0528043515      1
    261 0xe04e8afeae36d99dc370630f1333d43f87f52173      1
    262 0xe0c43cf26b0bacb408c6334468700179c61aeebf      1
    263 0xe1e71e8b336814014efc951e9cbd9432b2e7bb8d      1
    264 0xe36067a4c51ff6cb7fb6f97d1931bb27a468960b      1
    265 0xe434f59930e9cd2dfd2235ea86c9bfdb44f5bc0f      1
    266 0xe4a399c7a0b68a084fe9b859443a72d1e34f4895      1
    267 0xe4f642be846a12a3d2ded7e783293337465e5d1f      1
    268 0xe5050e39addf3475f0d7ebb17c6bde117f0590e2      1
    269 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    270 0xe6ba7ce1bbd7b811d891c2d4c09d7295bb96752c      1
    271 0xe82d3382efe2798d4abd2d824824990ccb75e4fe      1
    272 0xeb4c5c24468c3e666d01257b76a73326411b7928      1
    273 0xeb6ee7076521f9ef9da69705426d2335f27c5168      1
    274 0xebd56f0884a8ddb3ad47c767c5647738b04ed1ec      1
    275 0xec8982195f339e202c0450c7baa82fb201003222      1
    276 0xed8e0580629d079fb5242e4dedfea3687b685527      1
    277 0xee8dbe16568254450d890c1cb98180a770e82724      1
    278 0xefea7c883b74ef2865d953d8fa46d6e654b8ffdf      1
    279 0xf063be86ef61e4e9b6baa6e6123838da32639096      1
    280 0xf297ffe8bd936dbbef991c913ea4a3c7bb2f3bb2      1
    281 0xf2993590f381123cdf654529e41f915355ab6bc5      1
    282 0xf3aacd88b40478ee4b339d336e2327a9a3b42a84      1
    283 0xf58c03997171ec5e5ccaaba7338d0575d5616823      1
    284 0xf59a58fdc3f820941fa16f53e56cb11e7cb2841d      1
    285 0xf5a74a49825fddc6d331d4141c9a35f723709e08      1
    286 0xf5eb53e83a9fbe8491575b1174cee2f2989a43c1      1
    287 0xf61030d320e71256a43ec22839db345d80ac84b3      1
    288 0xf61b873a21d767ff0506e8b2630c4dde7216035d      1
    289 0xf737674eb90de5a376cb947bfd6e8f63635bbfbb      1
    290 0xf84f2f86be594dcccd4c192ab8058f9f73fb25e7      1
    291 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    292 0xf916b594fa568fb4cf5ca7f0ebeb11ffd9e296c5      1
    293 0xf9ff406304e9075d93a2328ce4550b87e0757a10      1
    294 0xfaa9dccf86f5cd4db872377c18368b5d58bc7b38      1
    295 0xfae772e7498b0cdfc4ea929da72333dcc0dd2517      1
    296 0xfb74eee69be0c2bd8fb42ee67b6b6c4e05cd9ddd      1
    297 0xfd9c5f1377692c158c56c150cb4c84ea1226f56e      1
    298 0xfecf9b009af216056d27759c1489f00fc62428e2      1
    299 0xff08d4d9ae7e2cd20a9c48c024b2e2a1dd8f7c85      1
    300 0xff7a752057d8b3444c94347b7195d29384df4e00      1

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
