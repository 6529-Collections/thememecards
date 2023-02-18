
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16656969.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:5996        Length:5996        Min.   :1   Length:5996       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:5996       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16657269 # https://etherscan.io/block/16657269
block_hash <- "0x620b69c5871258b2c827dbc9050ec11973b117a83d5962db4b2c267bde9fde4f"
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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=5,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=69,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_cdb      <- pick(snapshot, contracts=c("cdb"), address_remove=address_remove,address_pick=420,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 5 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    2 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    3 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    4 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    5 0xea39c551834d07ee2ee87f1ceff843c308e089af      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     2 0x0ca3a3b144148642f68b88bded521ce637db8f3e      1
     3 0x0fb586f584dca4f2ea2d8bfb19ab72bae35d6900      1
     4 0x15d4a76dd2253bef53d9346d840aff9fe385fb5b      1
     5 0x1854a7a0b4f9466dbeeb3279e247d4d0b0a06e3b      1
     6 0x215fc4d12967ddc2432a7b8936f2849740edb7bd      1
     7 0x25acc72796bdf4579755708fdbc8409622d224f7      1
     8 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
     9 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    10 0x30eedd4ea7e1959fc9d6879cd26f1d8baabbc5f6      1
    11 0x337101def3eeb6f06e071efe02216274507937bb      1
    12 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
    13 0x3927e502c865a2c873a735775e224930eadfd2e3      1
    14 0x3927f1b51afd205311bdddeb5646c4c6a309b2b2      1
    15 0x3d7d85485b37360c7ad7ce17e549fba5473a0c0c      1
    16 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    17 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
    18 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    19 0x50f8c08b0124092e1001b355f4b8ae2df85f715c      1
    20 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
    21 0x622e24d3e721fc386678c85792f48388c1150220      1
    22 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    23 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    24 0x69e68074f1aada957edd39c5eae0069973343f30      1
    25 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
    26 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    27 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
    28 0x76e6161f1fd846100140cbc111d0673b216a74c6      1
    29 0x7c8a0a4bab02b7c6928952fc806776bad68b4340      1
    30 0x7f1796071eb91aebda10a03115fa3cd95efdb25f      1
    31 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
    32 0x82139687faae8a29851902783e02e699de0e0846      1
    33 0x85c0c90946e3e959f537d01cebd93f97c9b5e372      1
    34 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    35 0x8a0c391ed12dfbb23d4a926a7c7353dcfcca8a47      1
    36 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    37 0x9acb83a514399c9f5201f55ddab61481d0430943      1
    38 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    39 0xa26e13cfc426d42480bec7874e27cd1e1089e32d      1
    40 0xa582484cc6016a298de57ff45b77ac286ebeac61      1
    41 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
    42 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    43 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    44 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    45 0xb05dec2370a17478e726452bbfcd093f64f5b900      1
    46 0xb40969f60bfa246a09593099dace2ddd543254a3      1
    47 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    48 0xb4d502361a3c6f823eeb9a99af09e110382206ee      1
    49 0xb53ee69b92ad6d12e6f3b1f849ad3e706e31a263      1
    50 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    51 0xb80e2273ccf80bdfd2e0fdae537f5b5f19535a55      1
    52 0xb948ea974cb95c9c73c51deac5d9e4464e769a44      1
    53 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    54 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    55 0xc2d9eb979f4679adc0a4328ac5caa096a03f18c3      1
    56 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    57 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    58 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    59 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    60 0xce435038720f5db23cc8627f825382dfe85a7e06      1
    61 0xd38a87d7b690323ef6883e887614502abcf9b1eb      1
    62 0xe3c3caaa8019aedf0c0833f7cfba0eac5fe82341      1
    63 0xf1914657d98373e3b3eadab8e82b0e0431e61d0a      1
    64 0xf271698bb09914dd7f54654ce1810cf4b0dcb123      1
    65 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    66 0xfa69519696e9d4abdf7d054c3ba44d32fe350ead      1
    67 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    68 0xfe4da73ff6543b17b2b18e0e5d94bc87bd79f527      1
    69 0xfe83a0a06d830e9e2119522538f8b440ebd42ff5      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 74 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x1566ae673ae80725bcce901b486c336e6acef465      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    17 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    18 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    19 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    20 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    21 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    22 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    28 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    29 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    30 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    33 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    34 0x69e68074f1aada957edd39c5eae0069973343f30      1
    35 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    36 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    37 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    38 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    39 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    40 0x82139687faae8a29851902783e02e699de0e0846      1
    41 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    42 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    43 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    44 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    45 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    46 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    49 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    50 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    51 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    52 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    53 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    54 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    55 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    56 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    57 0xbf814810b44978de273191fd612aa47f7b69d564      1
    58 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    59 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    60 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    61 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    62 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    63 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    64 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    65 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    66 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    67 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    68 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    69 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    70 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    71 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    72 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    73 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    74 0xfd22004806a6846ea67ad883356be810f0428793      1

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
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow CDB

``` r
c(allow_cdb) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 420 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x006a8a496889e9983a5256f34b608ee9569ba319      1
      2 0x00e952276c03e71b7b6193a67ace9b3e60546042      1
      3 0x013fb2ee63aaf302a5f349c8c73c4ee9646cc84c      1
      4 0x01b30c25733d1b4a3f4ff809816706e54393478e      1
      5 0x02306108713e99cf45dfea33c12438ccba96482d      1
      6 0x02675f7782bbeff300b8dd2bde263f65be6fe1f9      1
      7 0x02caa5c046d7b9c92e21caed72d887575adc01a9      1
      8 0x030dacaadebb91adebe1321e63d06b430ebdd0d6      1
      9 0x047501827c287d29ee4e6062f1f70be3d037d140      1
     10 0x047d684902e61788b3a0f50ffb1e9e1a4167f1dc      1
     11 0x06ac1f9f86520225b73efce4982c9d9505753251      1
     12 0x06e28c3c956d47eb163c8462f77cf52e3e00c2f8      1
     13 0x0738c77f9b1b61ad32ec9ac80f018547d5cbbc7c      1
     14 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
     15 0x0778e79130594fa32b0b3ec87e1d9f92af43bce7      1
     16 0x07c73af8440b6a35a271f146b137521db6e19df9      1
     17 0x085843dbde124ab8babd558fea60534962628338      1
     18 0x0a2bc233d223fa0911a4ab429e6c8960734e1757      1
     19 0x0a3df5dcea6f16cc3b0f6dac6831c09034940653      1
     20 0x0a5ffda07a44334777c563f9b81ae7bfbfd44ad1      1
     21 0x0a690b298f84d12414f5c8db7de1ece5a4605877      1
     22 0x0d815b175e1a937519239c9d1510e9cfbfb82f38      1
     23 0x0e1ea689e197d99a6d99712c9d09342155bc1dbd      1
     24 0x0f16b8fa33a660fcfe3c5d7b9503439d055fd39a      1
     25 0x0f18c529b51851079b348bcffd1a45c9426fa65d      1
     26 0x0fc030447c326df2f67f3a9d5f011e03313729a7      1
     27 0x103d90f1b6901f3ce4cd95543d6b8901f9629031      1
     28 0x109ed8499715a6bd152831c34b6d25a0e5d51d16      1
     29 0x10d0739e251f5c212312d023c9fcfe0fdcafca23      1
     30 0x10dfe5ed945fbb91b49ddd1810cf267420a8062d      1
     31 0x1129a84d77d78c628902e533f26a2d60a8383853      1
     32 0x118bf0693d1f51541e28253e67bb1d88ab24b7b8      1
     33 0x1306e209951b1bbb150eddd894b7ebdf29fd0066      1
     34 0x13c150622405bf2b5759663ce811b2b87f053601      1
     35 0x14ea47a7d5a323866df527055c3b9a5e758db9e4      1
     36 0x157d80da6b78d3532c0693f63680e71fb6dc6648      1
     37 0x15960e78d5546daca964045a3b39cd9406906c53      1
     38 0x15cb9364a90a366e2b8d0c1a46fa4045180705b2      1
     39 0x18181a21bb74a9de56d1fbd408c4fec175ca0b16      1
     40 0x187dc0a1fb41a056540b9d2b82bf0efeb0ff7d6a      1
     41 0x1a75bc1f2a78ee337f248dca1b9f2f5722d97937      1
     42 0x1af233f1f2cbf6e74c63758cb533268cb78fee5e      1
     43 0x1ba28ff812689905be578ce42fea80030bcfcf11      1
     44 0x1bd7a3b3e234da6b15c085211e1653d0ad8c0230      1
     45 0x1c1dd9a113c77daa9da14fbfdc9c879ec149c630      1
     46 0x1c2806b58dab61fa0c9c4b333df73b91a84f1857      1
     47 0x1c2b721fbfa0caf514c79f953bbcbad3c464eca7      1
     48 0x1cef61a36826b3f44f90aa42db33a50a2410f406      1
     49 0x1e3685ee600983bac782b182f245efd3bbb26455      1
     50 0x1ef6e22cce66525f3befbdb75000141182959ef2      1
     51 0x1f57b7cf79f16e1a4e54d2bb881851679370be44      1
     52 0x1f5d6b1ce0595c8e6111a50d624c80655234863e      1
     53 0x1f916bbf39ab189a9e3d9e1823a7b1a8e9e5f204      1
     54 0x1fef500586a301d843a8049b6fefb920b4888513      1
     55 0x212357eaa90cfc7f80bbf3d63d606cf2ab7fa129      1
     56 0x212ec31ef4c3af368db1ab4fc1b171703327c809      1
     57 0x23be42eeac7ab3db09ce1525801ace766a8e91d7      1
     58 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     59 0x247f6526a6b2ed8e27b60d2cd735d1f0a3ed7a1f      1
     60 0x2487203a1739e6db4dfa84b96bf18414c737efbb      1
     61 0x251b03afa4d4e686df9cdc551dfb2f0c6fd89892      1
     62 0x25b53b571e653ca400b6c4d050ee4e06f3d68589      1
     63 0x25f1f30606a61e9dd4369e23e2da11fae53349e9      1
     64 0x2658b7a45fef1983bccdb0e0a0e3f2a6420c6ea2      1
     65 0x27625358cf26ded992dc5a42a0567d4cb3ac71f7      1
     66 0x27e46e5c28d29cae26fc0a92acfcb3c9718d8ee0      1
     67 0x29372ba2824aec47c2198766ccb2558436239191      1
     68 0x295dd46fbf445521b6fa0629792bdeb69f7c9476      1
     69 0x29d5cea7d511810f3ff754886b898fce16a6d8fd      1
     70 0x2ac2a4b5774f124cdc778f966cd9bea563fb9f6a      1
     71 0x2bf8b209ad1466c34f18238df6c1143fdc7d1939      1
     72 0x2c7029cdb56a562dcaab77028824618b2598250c      1
     73 0x2d5a1bb523b5bd75f30b9aae3f487cf296898f76      1
     74 0x2e675eeae4747c248bfddbafaa3a8a2fdddaa44b      1
     75 0x2ea2e43e69eadbc017d5036bb061a214c7e736de      1
     76 0x2eabfac8c9e4b4ffd1b51b54d8754470d58fa56f      1
     77 0x2ec9b2d4e57ffc2c55d4da0982f13b5461a267e4      1
     78 0x2f3a53640025d9e02a74f7df70f4b0347fd3181a      1
     79 0x2fa510caf46f88ae7cab8daec696abbc299f8d20      1
     80 0x303c36e5c473bf36bf643debeb61c68f780641aa      1
     81 0x30809190a496804ae87a5271ef60fa5110dcc501      1
     82 0x316bea82e1dccd5d93955f4a08dbf305b26a6e2f      1
     83 0x318c0d318636589a025cdeb0bf155e2d550c1df3      1
     84 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     85 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     86 0x32fcc745555671b84ccf89cd573d3da69f95a971      1
     87 0x330dc8f805326d1050d604664defa04b2ac30b2d      1
     88 0x343792a12d2affc4cefd876db360109c2a909363      1
     89 0x35bbf64e77be6a966449e76570f1f8d757816802      1
     90 0x35de10e56b042d38b308ab62b10490305633a7f1      1
     91 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     92 0x3680eaf1f85bec9f120bcaaa9ef469fb849e1781      1
     93 0x36986e26980eb4973696bfad3d2ec7f3361eedb3      1
     94 0x37210031326c28dd989d69b0e9639f9c2adbdb6d      1
     95 0x376f5d3a4bc0462d7abb92d57756c52b3d90fca7      1
     96 0x37b9f3b921012d7ce2e181aea99af6d4dd22ada5      1
     97 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     98 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
     99 0x390edada69baff7636e4737704a4af999916b4fb      1
    100 0x39b364c3d4bc3d2b1172caa01ac28846b400bbd2      1
    101 0x3a27a89d203fdd6f241c43b4ccc667dff10f32a6      1
    102 0x3a2f26c50d660e890f706702acb97231478633c9      1
    103 0x3a6372b2013f9876a84761187d933dee0653e377      1
    104 0x3b3ff7e3a21815602a1062e43e390accf5fb5e7c      1
    105 0x3b60c726f8449ffd2eb038c64f7d39310215d523      1
    106 0x3b6750fdd854ed73cae240df7d16fbb3eedcd9cc      1
    107 0x3cc9063e7ac5fa8345e1f59bc32a470ccd30ca6d      1
    108 0x3d11c06d80d9b4c365af5d699bcf721daa67e6d8      1
    109 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
    110 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
    111 0x3e83920cacbb2ff5ba0a1473e8fd0b4dbef95b38      1
    112 0x3eab95e703c016e0fdb98204a445c104f642c319      1
    113 0x3fc145ea0fa4290ff0304c689f78dc8fc69788f7      1
    114 0x403089b876128b97012aee31e3f91b4f6cfebede      1
    115 0x4069d163d9ff94f4e7025f28ac3f3c5df3f7a149      1
    116 0x40edb3a1d959c927b5113b5a4f803443f552f006      1
    117 0x41b68202dc64904e09d2e4f0a94879c741f27db2      1
    118 0x420329675e355593a03c46dbd36e412ddeda3063      1
    119 0x42054afeb1f649dcfa9b15ddf43353acb4435ada      1
    120 0x420948e2c3a5d28302c68912c01433e696fba57f      1
    121 0x420ebc3ccd52b84eb852572fc6c8b69a811f10f3      1
    122 0x42a75fe5ab5978acf025c9bd913af7f7a494b819      1
    123 0x445efda9096c4dc12725d7f1b43ee046fd86e406      1
    124 0x44e1b8484f8cda2cf8b25f2cbf17cb6cc4d7cc3a      1
    125 0x47fa33034c9786b244d3a3ecc8a559483e585e65      1
    126 0x48e2f33ca34bb58b31abe1e38623bb63b5320024      1
    127 0x4a4109eab70da4bd28b277998c2c8820beb54e6a      1
    128 0x4b31e864d5f86eb0472931cba1b5056da52ad4d7      1
    129 0x4baad838e29adde3076d64478b408b63d5a53fc1      1
    130 0x4bb803af0a8d20589af609896aec956d096778e9      1
    131 0x4c2bad6ded85c73dec25930c4d88d72a93512eff      1
    132 0x4c54e5d272bf7baf7d53d71049e9a7059a18d10e      1
    133 0x4c98735222f436ed6c88dacd5ae4dd895c6c1a05      1
    134 0x4d828d6f643308cbf16ba7a67a676a2c2938acb2      1
    135 0x4d8838d4eeda7f2ced51f817fa03909da34bc834      1
    136 0x4e7a008c396e01b923afe3a54a95c7ccc5e37b6a      1
    137 0x4e9cca41646b60a6817a984c8721d8e7e783c7cc      1
    138 0x4f08620c53f2b92fca61c78ee502ad50ccecb10d      1
    139 0x4f4981bc89d5f701a38112c2fac4362c5d9e4a3d      1
    140 0x4f79378f667f2359558616fc1f7116bd0d613343      1
    141 0x506af9879816031824ece1bb35bfc3b1c94a2a4a      1
    142 0x50c74245da642223e191e90426abfe8e778fc523      1
    143 0x50d297cee576b7a9abd893580a2d0b5c49c3274d      1
    144 0x5190f30bc41d260fef2f2229c1a5c6ba41d72f3c      1
    145 0x5235133006257941036d069f0917ba33e9d43723      1
    146 0x52494350d30e420f3f16a3fb7a87e2b83938c11e      1
    147 0x52e70830785fb22d6ea975a4f56f563efc617daf      1
    148 0x5316f1a2594388c989273c353865bc43111a9bd4      1
    149 0x5382859146010ac16e142b6708f8e178e77f66bc      1
    150 0x54e35a069ff7916d594c4b7fd404bd7688f589da      1
    151 0x5511ce8264dff56edbc0dc1714f0e5be0dae635f      1
    152 0x55851007925b8336fc39fe3ed87ccc6005b15603      1
    153 0x55946b784215432ffe7713b6cefdd0990e84b79b      1
    154 0x55cf34223929a9d893c1ea402735a3a6fc6e5f74      1
    155 0x561de3f20c31c3c4f39026af138c9df35e7f348c      1
    156 0x56682c4f13d7f52189f47b6e565730aa326ae6af      1
    157 0x56e83e8fc74079a58662f443ac39de153f02504c      1
    158 0x56ffaf0c96b3c5c1e7b417b0251b0ee5891dc74d      1
    159 0x581e2463e67059523e7df3777203687d090a6794      1
    160 0x582c35a77cf0c4726d433e9ac83db858893ff718      1
    161 0x585ba4ba51818faf84e4dd823ac363907ed890d6      1
    162 0x588672a61fb89f2dcd9a70001f06e8b692567755      1
    163 0x58bef80423298c5c11b8922d001d3c0d952d9b06      1
    164 0x58e8cb60a7edbff828b3caf2467c2e75734116ba      1
    165 0x58ee91acc27f1dd3ae534a17553c024da7e37eb0      1
    166 0x59c0972ac731d39050e3dfc75295ac5f67601aa4      1
    167 0x5b93ff82faaf241c15997ea3975419dddd8362c5      1
    168 0x5bfec9629eec534eb128df9a2e9db7ab4c7b9497      1
    169 0x5c480fed58f2d00969e4cc68d3bd8cce99cdc40d      1
    170 0x5d4f9ac9b515d0909a8606ae613c8f93b73842d5      1
    171 0x5d75e4e8a07d4c91e5003749defb35d209410217      1
    172 0x5e4ba822d856ca3a189e5d312382a5453759fc49      1
    173 0x5ed2516e649926fef80b3fc8d6c58caf981f2932      1
    174 0x5eee3de376279ca08f485258210aae603fdd676a      1
    175 0x5f450cb8391586b739b433f427326acb923b5daf      1
    176 0x5fa8fa0d0f3d45c997e5d31441f56a4f2881085d      1
    177 0x5fb679403d10372c8ed27b4a5e88442c165241d0      1
    178 0x5fbc28977418224977d212ad521382d7825e4184      1
    179 0x6041f881358c71d64bc9253c9ba0391df69f7d98      1
    180 0x60c3b507482dd50d368995e6dc53b37e3c7ece23      1
    181 0x62b5d52aabbc880e757a7ed3c4d0d9fc0899d35b      1
    182 0x62d8630a5eb7f79450429b5672d8c1e26cde3b49      1
    183 0x64c4bf67f8782f1bca8a34172670c061edf7638c      1
    184 0x65739c97808cdab5bd8cb7fe36de41ae953972d2      1
    185 0x65cd64f936b25ba20b006bbd8c89ba420f3e6d3d      1
    186 0x666913e971e0de2d2ca685ee5197d162eb5b9c49      1
    187 0x66fb3d5280e33fef1b8a36df202688be15f2e514      1
    188 0x682aa10ae360dd84aff56e121d77bd6be0ca04f3      1
    189 0x68c5846901d926351632bcd7514ed1a1efd60f9a      1
    190 0x692d1fce318f98865203a256b1024e070295c6f4      1
    191 0x69d2aab15d6be4684ee52192c4c75c614073f293      1
    192 0x6cef15e37392f13d0873dc18497ca8087c681e01      1
    193 0x6d02a3a67601f5cb015a01b2c8c8e2a844595e7f      1
    194 0x6da155a1f05b2ca102a5345777987c5408d6a551      1
    195 0x6daf7e2b611b114aa1ace6b19d282ac293e574e8      1
    196 0x6f4152b9eb3b626baea2f549c387c5bc31d857cb      1
    197 0x7048b6b511d9e7ab68ac1ca104703bf1af14f699      1
    198 0x70eeddc9a50d0363da1a039d5368633950d82d9f      1
    199 0x711054b4c189d7d13a11a38e1133dca6227a22b3      1
    200 0x7117fb4e85286c159efa691a7699587ccd01e26e      1
    201 0x72e386c7d17dc26163840b8202c43c817d6cac1a      1
    202 0x74da634dea0b9cdf3a80e36943928d614f4ccd20      1
    203 0x7509cc94d13ff5365cd1159d0cd0599200b16041      1
    204 0x75863c5dd30fc162d7e6b30a6caf015cc4c066c6      1
    205 0x77fe62969bb2284f3aa3ee6db07e6d51866884ca      1
    206 0x78589dad2a38a92b6913c5e817c7e47104da6351      1
    207 0x78b5942e39add93e7aee98ea4440057acb527b49      1
    208 0x7901452659903ea4e59d7f88235cb2c51c518bba      1
    209 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
    210 0x7a24113ef17f916ec4a035980a1a9b0ca3e343b8      1
    211 0x7a65cd0ad11e7329f534b5b65113997cf75e3546      1
    212 0x7b640407513bc16167ef3450fd6339803982e976      1
    213 0x7c0830c2fc55e23d23eea9ae8536bdb43dcaba4c      1
    214 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    215 0x7d3f21bcb6c547737812a3213ea105192f9e151f      1
    216 0x7d7cbe01d681bce815e41c64405a181e11ee2954      1
    217 0x7ec1939150f1ca31800280ae89e31fef72b669de      1
    218 0x7f23c9047c7f29ec40ea742055ad6b5ca950b5ef      1
    219 0x80b9c8c776d582baf36aac01229d1603179a31f5      1
    220 0x81b2f8fc75bab64a6b144aa6d2faa127b4fa7fd9      1
    221 0x821020d081652df0a704822897c1fa064963304f      1
    222 0x8282d7e1a0ac8cebc764d5af744ab8fedc5f1e3a      1
    223 0x834f0e326d64a3e4dabd995923fe3122271791c7      1
    224 0x86f39177283138fd6f5e344dfb78675ba4759ada      1
    225 0x8756da913378b865cc6e5bbd8d403995a0b37567      1
    226 0x878975cf4a97774af6875b3b37b0920b83121f35      1
    227 0x87ba6ef5fab4a3f54440d2f2a7606024f6971280      1
    228 0x88368732e79ad01299025e6216bee288d2595d2a      1
    229 0x888c1b86000bbbdb2223a0b89e3c82aec9d94297      1
    230 0x890f0a0d307ba278e1cfa2e5f09c28fe96d7c6ce      1
    231 0x892857d3a0e6ca504da54470a0d71593525ebc22      1
    232 0x89992dbb585721a8f649ccad30b93978b019bf27      1
    233 0x8a18a7ea34a476018da24c385f07dc2508a45c50      1
    234 0x8a2dc70412d4e19f81bfa911594d933c58c02da2      1
    235 0x8b3477ca7445ec524dd534fa4a596c993e93675a      1
    236 0x8b5e90aa714c967efc8c286f46ce0fc822e1cfb0      1
    237 0x8be3fd8a423c38131aae0330b20ceec425e0e50e      1
    238 0x8cb377959625e693986c6adef82fff01d4d91af8      1
    239 0x8e3632743163f6a4d38a6953e071958f95d88fb3      1
    240 0x8f2d8527c94a7aef846321fffa47f06fa7230456      1
    241 0x8f86570581c1114a8a886117a8a30c77a6ffbc80      1
    242 0x9030784b5784d3b1cdcfb90ebc0438d51ee9c80b      1
    243 0x91163be6f088cd7bb4d2a7e8cd4b4dbff37041d8      1
    244 0x91f50b90632e33b70e295512a688dcef614fdd71      1
    245 0x922ee3b214e992b424ab419c49fa6df64768a1d5      1
    246 0x9338e03ea15949aec5905f595067fe3ec3e67c2a      1
    247 0x934da1f8c164c1b4ff3e45ff1d70783c3dafd547      1
    248 0x938acd1dca436f425b101694ea46c38b4bf1d2e4      1
    249 0x944d3225a911852a2562d362117f6de7e44cd2ae      1
    250 0x948fc7033882e8bbc402bbddac1fa2306f44a7d4      1
    251 0x965b813b302dfccdf6c2f676d59d7d3c960d3582      1
    252 0x96aa69713ced2f836333b98fa0487914d2399289      1
    253 0x96deff3672e4201f78e16614eae290b3aae0a6d8      1
    254 0x96e80d10db1f127ceecddc1c4205c80327a5c37e      1
    255 0x96f860ea6aa0c4fc1d7d1ffff2f81325f6eed5fc      1
    256 0x9754f5a0da7db723d0c808a1eec8c9d023768663      1
    257 0x98adbf6a876c13657e6eb95f558656b392801f2f      1
    258 0x98eef1e0484b05261a464457047850dde09c660e      1
    259 0x9953da7f2161866afaad3c844caaee35a262a001      1
    260 0x997197d6edfd538086a6475f5466c4096276d3ae      1
    261 0x99c16d5f932ceccfaf53143e85ae18f03256205d      1
    262 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    263 0x9ac4ab7830036e612522e3d2e627faed7360bf4c      1
    264 0x9be686698d1d4922a67808b8f5e84defd3c8c491      1
    265 0x9c4d96683af2329edd49c257d5411defff4b8d5c      1
    266 0x9d30c38eb5add35728e86d4fbe96593d07cb4bd7      1
    267 0x9dc2f55492b723b034f9d6809cf36cbbd54e635a      1
    268 0x9e59ce01bda22d5a153a6776505573e26d0f7753      1
    269 0x9e724c94692eea5777cad2e7ebf40c338169b268      1
    270 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    271 0x9fd6c1f52956b56f67e780b770e89ff0a0c9f65b      1
    272 0xa210cc608eefbb34ebea4525ed6515fe94d6036d      1
    273 0xa2490947b30258b522b7d6fd8fabec2d21c42d57      1
    274 0xa2ab5ed49c12975a686406a1421a5f2e0ece8987      1
    275 0xa2c8d741c21dfca2e211d852d930384a49e88756      1
    276 0xa307064b05b598c47aca897b1a42f0c84bfc52ac      1
    277 0xa428642f9cbb0e11f699bba752f309dd6079a351      1
    278 0xa47ece7a6c77091f89d36950b21d2575ce0eba40      1
    279 0xa527861319c2bcd2a3b5fd61d56809c6b018d1bd      1
    280 0xa7c0eca863a2c31ad0a8b640f1b83d805b496016      1
    281 0xa996d391f64158fb17fb71d6a4fad67df7410895      1
    282 0xaa54fa7c21aa518787208923d8d173f2b22658ff      1
    283 0xaa75b8c9e01ac2e9fc041a39d36a6536540b2072      1
    284 0xaaf67c8333fa0d65e38c11e3657cd36f79a6f3bd      1
    285 0xab450d37f5c8148f4125734c645f3e777a90f003      1
    286 0xab6ca2017548a170699890214bfd66583a0c1754      1
    287 0xac0a7656b5d62e7d54b11f8b5294ce0ac1bb7118      1
    288 0xac2ef512d940f35e5d86d0edf82aabb62de8facd      1
    289 0xacaf2b49c521c83d80bfe5876a8e2418c99dc435      1
    290 0xad3a159dcbe1c6537a8eca112302ce0f9c786ed6      1
    291 0xad3e6db03f708dc4f9f05d28d03a65a49586a299      1
    292 0xadadfa4213cb8baaad1abe99c8a863a4d0a94cc5      1
    293 0xade0b343d6f43e93f9269591f133fc7cbddbd33d      1
    294 0xae3a3d0a97fa75a340cc879700939699d775688c      1
    295 0xae73a7a38c6c04e2cc9ec3c416f48185ecd26f7b      1
    296 0xb08a7aeeb294123fcd99761a6114221daf30c39b      1
    297 0xb0b9214311564a77cee278c5e4705fb6ce3cc186      1
    298 0xb0ba5311dfa74510f7282953840f25403dcb2b68      1
    299 0xb235587b4cbf0df2a01a48bccb10d7e4993c38b6      1
    300 0xb2e714bf6b0cdde7f27316132fb612315f3faf91      1
    301 0xb3a4d18a160e1e96e704785e655c245ee649fb90      1
    302 0xb3af57ac61fc0e66839c8bc951a7a6bf24b8b082      1
    303 0xb463e6ce96a646d0aa037a681972562bc3c0d7f4      1
    304 0xb68bace12f832b72c3169b8de865e07806c32923      1
    305 0xb6e32c75a2083a38f6cce6e65756c0bf86d84200      1
    306 0xb70760bf57c3295999f978fe404f4ec906669330      1
    307 0xb91bb6b1264f241dd92922198426f7f158db4d3f      1
    308 0xb9ed9d42286803ec5d1c52984a7f5b610c76ceca      1
    309 0xbac0624c7d4a88ea1367082f1d0510dc925e4348      1
    310 0xbaf8716c9efba5638a1bce0d5a5b5fb45e56a56d      1
    311 0xbc584a19e0c5b1d8b8bf9bd0e6b64d81992848e2      1
    312 0xbca37906013aa0c1f6c006dfac9b74cf24775f8f      1
    313 0xbd10b101e852127e972506dd7907898c88ee0dc7      1
    314 0xbea99678972c180646a886dba2e5e70cec7105f5      1
    315 0xbf06d42287bafa1d13121e3116a6a9f5f1266181      1
    316 0xbf9e3689f0d31c3a1bd4ead19413ed135e999b32      1
    317 0xc0d40ed9ebe45f52b485e037a675d1f6a745b5da      1
    318 0xc20d87e7fd843362addb8b1029c1accb2f8193ad      1
    319 0xc2f43f2edcdf95b0f3ec0edfa25669b377a509b5      1
    320 0xc3b1f031098abfd8785f907bb282a5f51f972e48      1
    321 0xc4464dc30eb53437a1e84f380f813f61ae7e174d      1
    322 0xc457c62c3eda3fddd59f245a74fdcc5d6b641bf0      1
    323 0xc478cebfb27404624a9aca961a8fc9ceb94d691e      1
    324 0xc567ac610508dc26e129d9c6194b01ce399f44b3      1
    325 0xc6352ca10ed867c99c86bf60fa3a774f88331ff6      1
    326 0xc6d78254aa86397b4c971f76d203ffd25ad71eff      1
    327 0xc6e91644c50e0a90a5ad11216f47b3d8ce6acfc5      1
    328 0xc71974724c4b009c65c722af061900d7d26f5fed      1
    329 0xc7d88742fc88de6440fb0b7e6f992120bc966aa6      1
    330 0xc7fca07e9005f0b1327f1a2ccaaff3afc18cb936      1
    331 0xc7fd186724e85dde00f076973948e8a809ccfe20      1
    332 0xc7fe0d3624adbf8a5964f2a1db0f43a8591490dc      1
    333 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    334 0xc8c6c74cffba0db57faafaa511d85d386de02765      1
    335 0xc9cb0fee73f060db66d2693d92d75c825b1afdbf      1
    336 0xcc3f9646fe5d2e7a26473827c5f1dcd87b69acc2      1
    337 0xcc47fcbfdd6e558fb39cf5ef4d92e59890476b86      1
    338 0xcc55a426d4221d299a4586aaa0cef6e5c12783a4      1
    339 0xccc5131db50b631e0c6c4490b111c298dbc0c7c9      1
    340 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    341 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    342 0xcd3f4c42552f24d5d8b1f508f8b8d138b01af53f      1
    343 0xcdab2e0ee12eda091baa1e4316806ddef91880a9      1
    344 0xce357db97ffca9990947e197a8f6aa10ab84dbea      1
    345 0xce7a9f981a2a79c79340297edff7bb6b73f71913      1
    346 0xce848adb350f52db57ddd48923b1113fece939e5      1
    347 0xce94b87e9732b40513817ba3d02f765266ad25db      1
    348 0xcf495e2cc0b1a59ea07d71e322eced8c71bafa99      1
    349 0xcfbb54dcb5e041f0837509c52bd7089fa45804d7      1
    350 0xcffbdd3ff97d6c238c5fd3af6b62ddc82d80709c      1
    351 0xd03c8ad4bebd854a36010b864520efa0e4c700ee      1
    352 0xd09d3c607a5a77891eb715c9169e8b910b8151df      1
    353 0xd11cd3532f270808ba67d3e810c66be26b17e0e6      1
    354 0xd13c3feef1668ba5b6614f3b6c6f88e9aa962e9d      1
    355 0xd153b9bcfdccae5c762975603eabe2859615d961      1
    356 0xd30eb4f27acac4d903a92a12a29f3fe758a6849c      1
    357 0xd373a5d2c731e04065770511c74ff102198b3fe0      1
    358 0xd3fd068d8dfaa1c5509f70d16679d04e45412c7b      1
    359 0xd7272f37e384b594e885237aa29013cb49295e14      1
    360 0xd760d1655695c420d688bdfa43d0dfdcc5f98b4b      1
    361 0xd937ebc2d56367989c94360e5566a7c06a2e75e3      1
    362 0xd9e10e2d238d0f7f8c761dcf1c1b99156988d97f      1
    363 0xdabb1decdc90ce3af96c89902f335baddfb1ad16      1
    364 0xdafcd089794ecfd5e695b9fbb3692503bb04aeeb      1
    365 0xdb39efc48774a83ecc13829b615d48a1f09a3268      1
    366 0xdba9f878485cb575da527489c76ca355fe3d0144      1
    367 0xdddd34f88b475dae9fef76af218b00cca0d7a06a      1
    368 0xdde7b1103d7bb19982ef9c6d9a348a0c0ea7e132      1
    369 0xde13bb4ef14f3b083180245332d55285c932e57c      1
    370 0xdf9b0d723776972c5cae27fdb4f3ea827a5cf283      1
    371 0xe16c0e1bf3b75f67e83c9e47b9c0eb8bf1b99ccd      1
    372 0xe1b289da74a6a179b36db7def1026172cc3baa31      1
    373 0xe26f1356bfe4ac2f433c62660759be952f498003      1
    374 0xe2f4f70ab7daa110d322be56236efe473b03ddf2      1
    375 0xe44044f91c29fde60b618a11d4baa26519d7f750      1
    376 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    377 0xe5cff721e728f61a0a438fe9ce0d16cfb1dd690d      1
    378 0xe5e0aa97af3823efb54b9580456b9c8f45c7ad09      1
    379 0xe6445bd1d9674fe7fbedad2f56f3b1eaea1b029e      1
    380 0xe738052de24cdadb701be3a5f92516b72541f74b      1
    381 0xe82c4bf8837b43bdcd0c9f7c135d381f0650ab6c      1
    382 0xe831ee92a14819cea1639f6bc14e632bdde0e233      1
    383 0xe8e611d56297adbe1c1c0683261be58e6e4880a2      1
    384 0xe995a353a97a33e2dbac9e70ba6778db86728f4e      1
    385 0xe9a90a5bc79faf54a295729a30acd80bd880f0df      1
    386 0xec23db689ffb488aa09cb49984c205830842c5d8      1
    387 0xec4ea2c38a886d46df710f9affe351be6fb42b4d      1
    388 0xec4f93a158f27c3b3481f5754b0d9b1f3cf366d2      1
    389 0xecc5acc39f800f0e1c9db3d9b351c4fd47aab0c6      1
    390 0xece3a6e6902067fc7949306a9cae964663f81e7a      1
    391 0xee075d16773517479f0ddba8cbc974ae4e1e205c      1
    392 0xeea34a0a4eabef207e55f2126d869a247dcac65f      1
    393 0xeec5cbfa5e2d23db553c400e52e87c4d79e6aec5      1
    394 0xeefa78c53455138f3383a6afc004e0e5e4e3c107      1
    395 0xef30fa2138a725523451688279b11216b0505e98      1
    396 0xf22343eb7646c9139045f2ff01fb3049d0ed3685      1
    397 0xf266f6777219b30f7a5e67c3d595f797888f226a      1
    398 0xf2de13954cc05c91d4058db09e94100006bf5c9c      1
    399 0xf31866b1292b95f93d5c6030a01544bd1cd81cd9      1
    400 0xf36cc6a1f5958fc2804035e76b583ac4105fc7b3      1
    401 0xf3ea0a038688ea797ea5aa9379a3a17d7fdfdd1a      1
    402 0xf42805106a8b0ffe291690b3eccda31e24611bf4      1
    403 0xf4ed4fe99943513ea6e70ff576986311f416127c      1
    404 0xf4ff19c30c98533fd6d3cecf09b3d6802e470dd0      1
    405 0xf5993d2217ab9d7f1d685a21524449108b34ff69      1
    406 0xf5a9288eb6e86a3fcb717e7f13475d947f459e3a      1
    407 0xf5b9deb10215a6f6d268efca2509e5be7a25de2b      1
    408 0xf7babd771be09eb3aa6e1a96cec47c13a932399e      1
    409 0xf8a065f287d91d77cd626af38ffa220d9b552a2b      1
    410 0xf96b5ff95c512922a4997eda728e68c6d22b45e5      1
    411 0xfafa9145e56b74b12cb1557ada15d30407f3a55a      1
    412 0xfb797bdf4116bd4e3a09d633881335122075b0fc      1
    413 0xfb843f8c4992efdb6b42349c35f025ca55742d33      1
    414 0xfbdefb587455dbc22a6dfae3fbe58146e2906c74      1
    415 0xfc51e3baa9ebcd5d18d6489319c40c3b330b5148      1
    416 0xfd9cd26fc07a7c7e1c8cc0a0fd9fdf92ab2a6dfa      1
    417 0xfe5573c66273313034f7ff6050c54b5402553716      1
    418 0xff3879b8a363aed92a6eaba8f61f1a96a9ec3c1e      1
    419 0xff69df6369c917664760804a2e6d827749b0639d      1
    420 0xfff12ef0d6b2612da7fdb2bd9ecdc8ca139f8d8b      1

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
