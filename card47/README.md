
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16187669.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:25498       Length:25498       Min.   :1   Length:25498      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :3                     
         name          
     Length:25498      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16188469 # https://etherscan.io/block/16188469
block_hash <- "0xcbf3a904b9713f1f483f42469c40f1056c50fa92a3b0139c9e1f420c8ae37664"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4500 

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


allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_pick=20,address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_memes       <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=100,address_max=1)
allow_xcopy       <- pick(snapshot, contracts=c("SuperRare","SuperRare2","MaxPain","Grifters","XCopyEditions","KnownOrigin","KnownOrigin2","RightClickShare","Rarible","PixelChain"), address_remove=address_remove,address_pick=200,address_max=1)
```

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     2 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     3 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     4 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     5 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     6 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     7 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
     8 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     9 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    10 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    11 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    12 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    13 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    14 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    15 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    16 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    17 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    18 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    19 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    20 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    3 0xa45d6303369917528f17e14f080acf45d4edb776      1
    4 0xb5374cac6cad6b025246f19d20b0d4151b640558      1

## Allow Memes

``` r
c(allow_memes) %>%
tally() %T>%
readr::write_csv(file="allow_memes100.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00acf1f79ff43b1ff5b4ff28b537cbf27445869f      1
      2 0x019accbe7f0823598ea968b7271f39735016da9b      1
      3 0x026e3a7cd4ecf91a8ad116a6124880d876af3c99      1
      4 0x0974ff7e79d93c462421f23520c0e6ba300dcb97      1
      5 0x09bc19cd33faf805e55b708c2db3de87979b3950      1
      6 0x0a21a14fe203a267a13c3455abfc9d5f5ae982ee      1
      7 0x0d7661a7b7a89b40b6db128900557e4f1d1b3789      1
      8 0x0ebe5de476d0d4162d7f00c73a2851b31b202148      1
      9 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     10 0x172004734f85b1b6e3954c2f201dc4beaeb80c65      1
     11 0x1fb32a867ef50436db4953b768f5a3f80bc52446      1
     12 0x2112134feff010321a1692f3bda6c9373552306f      1
     13 0x280676491188f56fa386d9833d84702ac1e24c71      1
     14 0x297b408578957bd0e0d5458f1fc85920893689f3      1
     15 0x2a05941097fff7ef606bc62da1907719e1d8d435      1
     16 0x2b5faeffc4b8770144c29805de1f87fefa7e3156      1
     17 0x2d1d925f7144a3dd56913eb4bbd011b52dc2a2f3      1
     18 0x31f7d87f5f6a90f8a9ff6328e2c607b637f9c157      1
     19 0x325c13ec4f14bfb6bee50b14adbcb25afcaf5a01      1
     20 0x32ea9aa3bfa91431debec2bec8860807805da8cc      1
     21 0x386db8a4a2659cd4e1f76d3162c45e212b63a3de      1
     22 0x3c554c31c2f1938bdde970ac9add36f6264b55bd      1
     23 0x3c5f184d95579780236611f59ce97e90f716281a      1
     24 0x3d0a9aba6604663369ce736cb4a5f21eaf7faa31      1
     25 0x3e960c18474442fcf4e00ab1d9a6b9b5cc0a9777      1
     26 0x3f6d1e260fd0f69be8fe8ce1c425079b4ff4ae11      1
     27 0x3f7dd0b106b10d06bb4813715b4e24d9e626dd64      1
     28 0x4470e0f5a0a3969cb0d5aba71ad4e6c759dfc824      1
     29 0x44b76fd4eef6ff8327c3cd7359e2177de463e36e      1
     30 0x4d6667739da3bdec58c0ad3f41bb617edc256239      1
     31 0x4e142fe48c71092e78be1f1082fa8ce0cb15c354      1
     32 0x50e4a02362373d9dae47f340c82fa146e1d06158      1
     33 0x51658f42ef240a4eb0d7c7568e4b1eb8bf9ff8dd      1
     34 0x51733185d7d1f5f32c2bec2bf120b84b8d284a70      1
     35 0x541237a3e049f2ef1105694b491152f450aba4db      1
     36 0x5da7351a4cb03c33e11f51841bc614d985812821      1
     37 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
     38 0x60aae432fa6eeab28b18e0bc3652cdb04f1cb889      1
     39 0x618d17fa59c67aea20a88dc75d4e28e728a6ff28      1
     40 0x6281a2ba2120ee1fcc6100becafe472291ba8e6f      1
     41 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
     42 0x688b6650f9e1b5d766b5161c0ea3c56c19c95adf      1
     43 0x6a8c69dc2040c1c594be2f3947e15669536dd59f      1
     44 0x6acd98b7f9d818699aceb70f5e87fe148ccbb988      1
     45 0x6b8f5e3aa817dac35d211342819fc60d99e5f0fe      1
     46 0x6dc83272ac6e09044cbab1e8f279e908b25d0c4b      1
     47 0x6f8268368558c7eda236221443c1028677bf9c40      1
     48 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
     49 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     50 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
     51 0x7bef8662356116cb436429f47e53322b711f4e42      1
     52 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
     53 0x7dd3005f938feecf091151c0e4f5f53327b035a2      1
     54 0x7ef61cacd0c785eacdfe17649d1c5bcba676a858      1
     55 0x8037cfedb47d493a391dad76c4f60b8927cb8108      1
     56 0x8271df8a754d74894c91c1ae7ca8d5d4c23f1873      1
     57 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
     58 0x8d1919f9f69911b0c6630703f1ab5da3d3faf1ca      1
     59 0x8d2b16acddc3442e6f15bda256ec3ed9905c4de4      1
     60 0x8efe0c9face897246a7b7c5274a32f1721b30fe8      1
     61 0x91b49b932dd4b88523a390de9c4b964d463376a0      1
     62 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
     63 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
     64 0x959cb1897c18d4be61f365eb25893e0cdbb21401      1
     65 0x967be09d0652a0be582f217630963df129157d55      1
     66 0x96fd61202a698ee3eac21e247a6b209ea5ffeb91      1
     67 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
     68 0x9aa79e8afa349dec0b973203232089a5855bedba      1
     69 0x9ed96691c75bfd0f738838ad3516071b33bec557      1
     70 0xa17bfb3a816996ac3a1e26dddcc7850663548c16      1
     71 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
     72 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
     73 0xafa14280490a031dfa60bf2368abf9c5b650d31f      1
     74 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
     75 0xb4c1d3f047583c596a0dbf423b37118f42da97c1      1
     76 0xb5beebbfb568be3d5d7afc7c35cac5bc517a1fa4      1
     77 0xb697552062ea87b2026199f389bfab4c8e41897e      1
     78 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
     79 0xbf32569aefb2b39aa86f057d34f68edb867cd781      1
     80 0xc3a538864600c21fd57eb45cdaca0f1665dccd8f      1
     81 0xc5167c971a6c92f74b7999fcd3c66c1ea68ba32b      1
     82 0xc55d765bbd35e495ea519f0a170dedd30363e114      1
     83 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
     84 0xc81635abbf6ec73d0271f237a78b6456d6766132      1
     85 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
     86 0xccd104aec182930e8e95a1d0fe5a60c69e2103e7      1
     87 0xcddb35f12845511e4aa5d624eb7378220701ea4e      1
     88 0xd598e0ccbbc94714422d544f1caf162234c296ba      1
     89 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
     90 0xdc76cd25977e0a5ae17155770273ad58648900d3      1
     91 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
     92 0xe120eddd8a1fea7d73aad75d8ed8406988b2c98d      1
     93 0xe1fecf996d902e7e607cc40ba7e0880658c13123      1
     94 0xea88730cafec7a18685712b1ce200301c7c0b754      1
     95 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
     96 0xf253c885f7e5f2818c03547b16f5e9068d4735aa      1
     97 0xf81489f074a9f70c294164e07692559269f3defc      1
     98 0xf868a2de45bc38844b16c9e63fda5e1de1d54a22      1
     99 0xf9f40128e49d2941fc7c906a9eca8bb65b54d60d      1
    100 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1

## Allow Artist

``` r
c(allow_xcopy) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x020ef67cc7338689d34fbafa4446990ff1dcf4f8      1
      2 0x027cae2ed1a23350a751452e907b4120330f9762      1
      3 0x0396e41347f6a8ca45b4a3efa82c436786dd44b5      1
      4 0x03e97a7fe5325a1fe2315a9ba00db9a0eef09928      1
      5 0x04319b5c99383b857d66627b186035027757e7e8      1
      6 0x04a806c901f5edba0584dbfda6882fdcbfc9df35      1
      7 0x04edd4261d178ee5f202afe1475c1738d03112c5      1
      8 0x07ec2140b69fb661e09905efb8729050e6d15c2d      1
      9 0x087f6883133b0070e5d60d74b0820c7be4f69391      1
     10 0x0966d26521c18e82d11c40d64d3d1853ced5e707      1
     11 0x09ba0d8c6e10147660714388d4857260d9bd7958      1
     12 0x0a9bc3b0d19bca983cd9a6d251dc5920e87b223e      1
     13 0x0b0476e1901b4fb88b527df234e9f1b4626713a2      1
     14 0x0b9ef309141bb050238d8a73d1fed0dec50a5405      1
     15 0x0ba322e5092a07cf58affbe9adc198e23eb722ce      1
     16 0x0ce390f18af702cca546297845a4a51d102123cf      1
     17 0x0d1d74535bcabda2da2cff5a53c2b899901d423b      1
     18 0x0db7b27b7a3ea8035d3f66f3ba7532221ef06abe      1
     19 0x1020a435816b2d335e36347ca86fb8bd99be9cd5      1
     20 0x13b3a50f3947476eda74fe191344524e2d2d28e5      1
     21 0x151dcb7d7c0bae08ff0f99469a7a03470ca9796e      1
     22 0x15e08516f63d4aa4e9392d95d58a3139bcd15a54      1
     23 0x1729e03b1e63c62aa278d9577ae2c6e9c4c6db80      1
     24 0x17735ad53064ebc0456622e0309bb4abb3dcbe5b      1
     25 0x1854a7a0b4f9466dbeeb3279e247d4d0b0a06e3b      1
     26 0x196d04f6ed4b3d4a3f08412f3190430c60b295a7      1
     27 0x1b4966114f3a7a4cb21c7e1d0a70567a29f82c34      1
     28 0x1c4bffe778cba14d253c471900bd8deec0deb428      1
     29 0x1e5139c78050d014f05968dc5c0755dae958481b      1
     30 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
     31 0x229fd4fc7ca05e61d626ea0d8fe2622ab01fcf6f      1
     32 0x22d95ba640e9f505622702443ef4b9d0e7e2cb6f      1
     33 0x22f65b617ab464ec3a98ef59073d47efaab38082      1
     34 0x26fca9a99e95be90447a25a1da852d870417aac9      1
     35 0x27493616036728733146e8b9886029683bcb9d24      1
     36 0x2b333ae41632c0266ae275e811bb5eeb3f21fd03      1
     37 0x2b564248d8f4fd425f3d01a1c36817deaac159be      1
     38 0x2b729be9179971e5f6d9962e73944ad992813584      1
     39 0x2ba77503ba95cda28a4015c73ec88b60516fb91d      1
     40 0x2c31159f1497aed2cf9c028e55416f3050db1dff      1
     41 0x2c82523d71d2d6085ff99ccfdba88cfc3e806d1d      1
     42 0x2cab11905593b47bbde0a579f586ba76f7ae18a5      1
     43 0x2ea48545b5f082969e02a973500024252b2d8fcf      1
     44 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     45 0x2edef9b9a483c206b3d966b318cdc6453f0dc2a5      1
     46 0x2f2fe67a1931bbb07915d7682a97a6e6f403b425      1
     47 0x30e474d8bf65c9642ad297e83bf389ec51b674c9      1
     48 0x3215d92f7f2a9f135df283202bfb713c7cd572d5      1
     49 0x340ee74b7257c6b11b7bf47fd279558ea9e143f8      1
     50 0x3750a6de42f4795aaad99687bd7c46223044c54d      1
     51 0x38b2739bcb869494cc7953c79c97e3bcad7eac04      1
     52 0x39eac3f1f1b22ec172feabf906880a795dd69933      1
     53 0x39fdd3fc7c2111009fae39473fefa7d197ea3c74      1
     54 0x3a89cd7d1cf3eb657f70954f0011548675c7132e      1
     55 0x3bf7cfed5a692cd9945decaa7770a8857b0f8a0c      1
     56 0x3c0c18e6b03d28737fa0bd42647d3845e0a70bba      1
     57 0x3cc8837cd778d12021a4e7baabab0e06d1d1fed2      1
     58 0x3f4ac5eacd9cb45a4ec9564b7a49f4950a9689db      1
     59 0x3fb3ea7cc848644193f4a1ab84b63791139d6655      1
     60 0x3fdbeedcbfd67cbc00fc169fcf557f77ea4ad4ed      1
     61 0x404b1014de52cd38f923f03cb49df783fdd3b7d0      1
     62 0x411d8c277b9fed4f543a69e737b3d997c04478bf      1
     63 0x425af9087848d811bb6b3f50a9624ad0b664ee86      1
     64 0x42f84b1f43e4213e337768e376da87ff99a5b58b      1
     65 0x4343d1868032c1dd017b3f52aa4df0f91fa8aeaa      1
     66 0x45fc99adb7179c314f5a014c8688d73cd96b252c      1
     67 0x463ff9b3abf5787b0c3103a559addbb4050c1ded      1
     68 0x48149fb00f18389b5c5bed7bbc6e806d98b4579d      1
     69 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     70 0x48ff7d77561b7a8d08a27fda13d054f37f7b1e00      1
     71 0x4bb7eceeb36395deb86a42be19fc1440a23b5ea0      1
     72 0x4e948c19640c00fa1d2538ad3e9da14ab7320a14      1
     73 0x4fed3126bf1f6ca86a1c603242bda9acf3ad6819      1
     74 0x539e72d521d4a6815f92985a5c060c0ec2c53b5c      1
     75 0x546404846d41ef75e09fb170a9777ec198dfdb64      1
     76 0x56c2eee0ac6d6618e83c07137557a0bb1c3a866d      1
     77 0x58b3eb75b8390badc945312ab1d594aa947577ad      1
     78 0x58d4ffeaaf65830daddd82506402e8557c804225      1
     79 0x5ea9681c3ab9b5739810f8b91ae65ec47de62119      1
     80 0x60448aecf3543a2f7cc4cc0cf00656a06279fabd      1
     81 0x610ff408a190ad7564883223b8b6e509ceae5c7a      1
     82 0x669d54e068c50567f9d234076086cc8fdfb8b072      1
     83 0x671675ee5a9ebc55c0409a00c0ea09c5d30a1e9c      1
     84 0x68f4b110b6862f2341d8a869a79025029f945bf2      1
     85 0x695626a661831a618ae846482fe8abf6c8d4529c      1
     86 0x69f92216eb974592bb9ea4e4f1da543bcbe7e03a      1
     87 0x6a933563fe3bf79bd03cbdb606ac25b816a7bd8b      1
     88 0x6afbeecc9d44e3a4418054c93186e43457384fb0      1
     89 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     90 0x6dbf4c620c8fd522c5f3de5cf8ef12394dae7170      1
     91 0x6fa02d0dfa8a172f4fb94952a282180acb788b62      1
     92 0x711cf786057416972671989c06d5a9c4e546c7a8      1
     93 0x718cdadb5d3c6a505b58018c98a6049449a14f63      1
     94 0x738027f2d60d05b3b136893fd9c97a75f737bb8e      1
     95 0x739d1cb163ee212c45dac9ed7a063395465200e5      1
     96 0x749412097da7f18375afb3825b8431d58832fb25      1
     97 0x74e2560995ba0b3e27539cea74d40b784f54689c      1
     98 0x761a4c362145de4bb4a91b917145be8c9a08d224      1
     99 0x7752b30ebbc753cab112298db890c83dcdc500bf      1
    100 0x79794358f2880696e5514ceb1d6cc5fe71cdecc1      1
    101 0x7a6597adf4ba4e405bf5bbe0f5976dc933b089ae      1
    102 0x7b7245c21845c30690c3aab79dee624d7f2ba22d      1
    103 0x7b7d22ce97cf46574e76551b831b357ef2385df0      1
    104 0x7bcb056e42727478fcf39daca7db71eaca783096      1
    105 0x7daef0084b8bb7f845e4d7ec0bdf4bb0992c0621      1
    106 0x7ee3acaccc605e4de444ec630083f739b3a73d6e      1
    107 0x7efc61570e1c1091f253480356b94218a3cad19a      1
    108 0x8063b817ad730fa22f538529a1d77fda75ee1dd0      1
    109 0x829ba9adce5735facae6ace102bbd22cb9c734b4      1
    110 0x83c8417e1fb748fe7113693deea5dd6938245702      1
    111 0x8486ea4c42c36b83924e1550d9803dcd1b82527e      1
    112 0x866137af8b36fe5163bf51b90febe2153a2c2c6f      1
    113 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    114 0x88826f90df1f3d6f13245ccb918b5800e7e519c5      1
    115 0x8950d9117c136b29a9b1ae8cd38db72226404243      1
    116 0x895d49ea015b1e9da4af776239d0f8b029d660b2      1
    117 0x8ad272ac86c6c88683d9a60eb8ed57e6c304bb0c      1
    118 0x8d1fd1f90568203bfcda6c89107a67fdf3126788      1
    119 0x8dc22f2e5d71ee3806cfe3dab6919fdde30bf802      1
    120 0x8e726c319e9f6871004f6956472e59fbb2f53fb0      1
    121 0x8f1ba6ae944d9d02ee23406ac1ade5362ff86886      1
    122 0x8fac841807e21807f511daf3c04a34cd78661f4c      1
    123 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
    124 0x953bb3aa4671b859298d98f70890b510176add63      1
    125 0x954bf4f420a1dbf0b0506be02d69a9e273307442      1
    126 0x98033e1bd361803cb08122e27e55240a3f581e38      1
    127 0x9a8367020c33592e14a6c04a09362c960409f796      1
    128 0x9bf904191bf0c1a66f8e0f21d88621c358008734      1
    129 0x9dbdad4abcc48610b22d56a6bdd1aa7f97171b06      1
    130 0x9ec4bdf1fa30738faa83f950be25bb1b87f853dd      1
    131 0xa05cd277ba50ec72fc41917437a6c09e39b99bbb      1
    132 0xa09f8241f0837ef6769a6d61350f820a92b33a96      1
    133 0xa232abd055f5c76730109d5f7b09597bd2242506      1
    134 0xa3c277b8f35881cbdb017e52bcc376b3ce8f21da      1
    135 0xa4f0670468dfb825e6c2b5571e2c97d0844190a3      1
    136 0xa691c1ee999577c6e6106caad8937a88c66f4c0f      1
    137 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    138 0xa8ffccaeb6a2db71a9678117fdda6ec2906d8112      1
    139 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    140 0xa9b84d262d9e6fc7aedd8e5a9be537c5c5cdfb7c      1
    141 0xaa34dde115e593dd26d0e6b250321803371dfc95      1
    142 0xaa6e633be7fe76331b67fa4a897f803d79fe53b3      1
    143 0xab262b8f5e4e1a876d1ab6a2d6ccb16b7fb5fa46      1
    144 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    145 0xac88715cdfd26f3aaadd0e17ce2011728f1c5748      1
    146 0xad532e06fad45f1592cce514890130cb74802977      1
    147 0xaf5a5dc780d1c9692c5b5306320a2e2700b2e9fb      1
    148 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    149 0xb733e52dff6d056fad688428d96cfc887b43b5da      1
    150 0xb8a9e697757b3767047e27c9b1cdaa2a2ef0c0d8      1
    151 0xb92c8c16ede4649b6ea6015beaf2f34973dbbd44      1
    152 0xba0b5b765d3d638eb560c7e08fe17f360d8e5005      1
    153 0xc15fa4b017da84f29defd582f86e99e1e6437c6a      1
    154 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    155 0xc41530c522fe7437f1bb15f61627d447670c2311      1
    156 0xc4bafdc0a6b7c1339055cea9dfd433b28815ae78      1
    157 0xc4f354b9819a2a289d27e1939aec23da9ec4a8bc      1
    158 0xc61043ed6527f71ffe26e3464146eb0b32dc9591      1
    159 0xc6a6a8f3c3f8f47269019c16f9ea087a3fc83c35      1
    160 0xc81efdfb56687a276935a15dfb08e33fa500db7e      1
    161 0xc8ece128e77dfe3a3bbd2c7d54101f2238f8b611      1
    162 0xca0cac48f1fe28a464b1a88577928495d47a9244      1
    163 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    164 0xcb89e78ebed526466fdb69c74a2655f2f913b544      1
    165 0xcbf37ce6c0b3f11872eecd696495fa4d7c699a5e      1
    166 0xcc7abc949bdf6e3e603a282ae9a64e8b43a96c4a      1
    167 0xcd773fc69705172d732e90b501211ed49297e370      1
    168 0xcdcb4ad8e3a5ed0c1ba6334091486afc5a7c93d2      1
    169 0xcec1930935e6148c6d74a9a8d18e99c216a75b9a      1
    170 0xcf1720f21a42af057e55addea70cc56c38d37baa      1
    171 0xcfc310d6b70777edac66117cb31fd14064c9f4fa      1
    172 0xd030a8f65c71152a2eea3ecf26ecc1bb266a013f      1
    173 0xd160b9a8c919cada4b1589789ba119d9381bfc72      1
    174 0xd288767f458ea4456f6f20e2b08303932f9f4475      1
    175 0xd7997b633327b15681bf19d6fd39986d9be2bf3f      1
    176 0xd7bad5eff26389b4ee7822690207b13106e03d43      1
    177 0xd83a5e7bd6eddf35431e0b2aa2680449541e7444      1
    178 0xd8cde9b0bba2ce08eaf8ca7198156c08a5451cb3      1
    179 0xd95f56179719b7f6fc1c4f1e7beba28c9e806935      1
    180 0xdb3be154aeb741867196a918fa2bd5f704dc022a      1
    181 0xdf6837ab43ea826b103dc025b17dba6f0fd9fcfd      1
    182 0xe0293be66b919120da50aa331e9c600f624781d6      1
    183 0xe1736b2ee7f1515ad7547485123de006ed093d52      1
    184 0xe21a6fe8df07a17f4424e103496705db8c9da526      1
    185 0xe2b2793d51c652106807caf9b56db81f71e7e280      1
    186 0xe3475bf2ceea9ed2560d5f68118b9547819fdecf      1
    187 0xe5a2e9a2bd68891441d96cf53b0ebc3db261d9a4      1
    188 0xe8bd506a49811d89dc7ec6ac691cb871b0530be7      1
    189 0xe9976059d92d5bdf8464b55aa048c4343fa01886      1
    190 0xedad1046c0876d4f92b4f2b20a6068543c36de5a      1
    191 0xee74258438bcd2882fe907c91a5371dc5dd5b0ee      1
    192 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    193 0xeebac42eb9ddce30891b4689831c4088b089223c      1
    194 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1
    195 0xf2626eadce87913c29e63b95e39552e1bbe26b44      1
    196 0xf5a6b235b00d56d583b908b6bbc6b71b6d036c60      1
    197 0xf6330de94b60a063f547f7b9dfaa5a624cbda702      1
    198 0xfab3ac52068380ecff48365c9aab0dab019eaadd      1
    199 0xfd3763ee6697c73c5415ef998d710ad1c8f0b06f      1
    200 0xfe64459ba83a0e5573f31c1c205f986eb7bca46d      1

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
