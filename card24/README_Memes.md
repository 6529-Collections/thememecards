---
output: rmarkdown::github_document
editor_options: 
  chunk_output_type: console
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r include=FALSE}
knitr::opts_chunk$set(warning=FALSE, comment="")
library(tidyverse)
library(magrittr)
```

## Data
```{r}
fn <- "15661916.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

## Seed
```{r}
block <- 15666928 # https://etherscan.io/block/15666928
block_hash <- "0x026fa289080c65a6a19368a1f810908c0afe7917927af11322b700e1959ddf64"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

## Code
```{r}
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

```{r}
base::set.seed(seed)

address_remove <- c(
  "0x3a3548e060be10c2614d0a4cb0c03cc9093fd799",
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x896b94f4f27f12369698c302e2049cae86936bbb",
"0x28b8d4f7516a112e2e2fd462293a1c27cde327a7",
"0x8f8b4759dc93ca55bd6997df719f20f581f10f5c",
"0x982d3c5223f6b5794fccb3208eb164d042cf2526",
"0xae0d16586e5d60d334624c115216a52b9b1a0335",
"0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9",
"0x129c6695dfe7a906bd8fda202d26dfff601f83a4",
"0x0a98f97e89743406a8611e1b4219a073b60ffed3",
"0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b",
"0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2",
"0x1566ae673ae80725bcce901b486c336e6acef465",
"0xee958e45f3464d712b8830deb5875c8ac105f698",
"0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d",
"0xc2419841dcb9a0f8906d06463ae24e00e1470846",
"0xb5374cac6cad6b025246f19d20b0d4151b640558",
"0x64f7de90dc79d775703bbec66a1591c7a26a22f0",
"0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5",
"0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f",
"0x40d2f4399d23f9afb82d0a6b73055f13208614f9",
"0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979",
"0xae72c6a6fad9fa9d82d089e1ebf73b3043855425",
"0x76db02500f7631d57bc2dcdca9d4cf782b99e119",
"0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f",
"0xfd22004806a6846ea67ad883356be810f0428793",
"0xcaa1c396e70384db58dd33be74b26fb119e98c3a",
"0x48464efe55fbcae8ae0c992b306afcf21d4910cf",
"0xf15a6b54e68884d27e1bebb1624d70c227b7d04b",
"0xf2bef831670df52ae5492dcaf6ae62aac86f6cc7",
"0xdb561a899557404581e6180fe6d4178577dc117b",
"0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b",
"0xea39c551834d07ee2ee87f1ceff843c308e089af",
"0xbbdd72fcce73c2626719be00259ddffef0d5673d",
"0x248458947c120ca057ec028b3fe7e4b3f26fdb3d",
"0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1",
"0x6f0735bf1e6c69030d6990cdd580345b370eb50a",
"0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8",
"0x43b0bd27d5016e969193412933387b0dd4cf3e0a",
"0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf",
"0x53006f95def268f88dc1b8216654ab56f3afd052",
"0xab2056903a7b62bac46f45a3d7a70ac799ca88cb",
"0xc26012491b9dfb2e6f2cb0305673e212721d5950",
"0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804",
"0x32d53a21813debfcb33a2b1ff2a396bd3a06f818",
"0x69cb3b1de24e08f1cfc2994171b6c6930498f750",
"0xe3b41ae8785e4107cc69f988042ff4a66a367fac",
"0x1c29dcaa0cad96ca3f60d414c7e2e47c99cd7bdd",
"0xbba3ced54477c12fdf16d7009771affc7a8c9ba1",
"0x80a1c9fdc26199a69d190ebc8ad287ef48758977",
"0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e",
"0xba4575ea27041d99e6614ec02318f1e23a623fe2",
"0x9dbd781eeba135ad2a779926880adc89196a3265",
"0xc762b1081c56b3fa487c7372f7284d9558a84859",
"0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31",
"0x69e68074f1aada957edd39c5eae0069973343f30",
"0x8476b6a8aa0b4037e69e79f116f662aa0096b0c0",
"0x8ea76483c888f5bda7d96cab9839488f691daf78",
"0x22fbaa2dd20b848084545515fb04b9c846942779",
"0x45855a3f4404aa08ffe14a366c75663f4ded2fac",
"0x3cb63b82d778105e43f064ed739b0655f1f0fb87",
"0x7546c60ae8d65dc6dd7a0f61c169818059ef49db",
"0x61d9d9cc8c3203dab7100ea79ced77587201c990",
"0xe359ab04cec41ac8c62bc5016c10c749c7de5480",
"0x5df5342342701b8ae5bce28f74ebb73b5fc13a54",
"0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9",
"0x8ba68cfe71550efc8988d81d040473709b7f9218",
"0x665654f2d2a151be2d0f8e3697e6ce780f732af2",
"0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2",
"0xe7c5ef2cee9c15b04fc7dea61a1ec443dd8e7fd1",
"0xa32f90b21d11561d31ff604745907acc77fb67e3",
"0x575f6540c16a72696c14a17fa64f049992d661ab",
"0x82139687faae8a29851902783e02e699de0e0846",
"0x699990a8e7ada9e92c932d6e8fb365024fc74b43",
"0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e",
"0xd40b63bf04a44e43fbfe5784bcf22acaab34a180",
"0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81",
"0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27",
"0x04df8d02f912d34fef12a1b0488ee56fd6f7416c",
"0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33",
"0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225",
"0x01d5fba02d3daad61e6c42b2e15771b06d19e557",
"0x0f9ab846449ca068f9714818a6ed0da5d819e9e4",
"0x1b6265a40839a331ace2d81bc82b6104703c0426",
"0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875",
"0x34b93462e65303f3857460971584fd0d908f2f45",
"0x381334a57eb45ee79183ed126c7a688d600240e3",
"0x4470e0f5a0a3969cb0d5aba71ad4e6c759dfc824",
"0x46e6aa05e0867d5f0feb749e81e005f5567ab317",
"0x47836f027d427e78c06f24eabfdd1d2089fdfd5a",
"0x4e2f4d43bb625b4fc17daf7c0ecec20bf1c86f1d",
"0x4ea7cc46443cbfd0ae19aec35f08dad83477a52d",
"0x4f492b2a3ed9bf0c65d508dab4f7c5f5c04ca6c3",
"0x4f7106c4c161e138b1a39f47c91c4f1043437fb2",
"0x5f9d41289ad44d17e6c51f889276999112e4fffc",
"0x6556751caf10474b9bd8b31ee4b0bb4420aaffb4",
"0x6850dd83a896ab7a1b191ee377cb46b708d4e515",
"0x6b9bcb0b854d7c7430baa55167d367d28a8dbf1b",
"0x78089705ed807984cf3edf9149c9369e8fabb9be",
"0x7fee3d50ae036f3e72071ddba811f58472995edc",
"0x834a9484537c2760ef6d6e6736d1ea800d67966c",
"0x860f0aa48ec759df1034d72e0311482a8b01db83",
"0x8a716e03a2bf21213903bfddb073a68998c722b1",
"0x922d1874a9984cca520a6c078f9856a158442f57",
"0x931e8194d361571cc8476b15f8ce2f6e72d593f5",
"0x98f9626154f44e3f30c112e9bb48b0678568b916",
"0x9f6ae0370d74f0e591c64cec4a8ae0d627817014",
"0x9fdaf0bd765561fbd609ea28ea67a39054cb28bb",
"0xa04f4a4b7306cb72f30828834db01699362a4989",
"0xa3e49fea82d17326636cece9c8c090edf013dccb",
"0xb209e4bd577cecb120fcd1797ee399ddd6533ac5",
"0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985",
"0xc07a67f50eb5bf18fd0c140c702a3032faa3681c",
"0xccc9bdd130f0c03fa5d12b9a85e9e66b087457ec",
"0xceb775d4b09c2fba2fe51efcab6e39a7da1528c3",
"0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a",
"0xd4012980ef607f79b839095781a31cb2595461cf",
"0xe560646ef7a69400974d85e818bc0e054bde65c1",
"0xe96eb4507a1d162bbb99301fe592d310e9489e40",
"0xf0fc534b6b655a4d3a895e055f57b0f1aa5d41cd",
"0xf5c2f167f6ed337d66180300fe476ac501b94d97",
"0xf8ad3f88b0e0d177aa8c5e6be1e13410fd41cdc7",
"0xfc7030fd3e3ca7541fd4f4a71b9d7b7243e83a37",
"0xfe0174255e410defaaf58b06e009e0ebcd74db59",
"0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42",
"0x54913cc8ea17731d62589039dd0152f306473843",
"0xcd241563f7288b5eb2b9b715f736232889b62d8b",
"0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd",
"0xd1f6e5592361257aef68b96df42aef064080c5cc",
"0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9",
"0x16dee223fc168abff7b979813cdf15866eed7e8d",
"0x19e7a132bd4b42f580f546197f42e19c42cdfe6c",
"0x886478d3cf9581b624cb35b5446693fc8a58b787",
"0x3fa5a25f48ba1b736761706801be4f639ca4853e",
"0xc7bb15c11595c877302ddfb330a4082d92f5bcd7",
"0x1430997022e8ccf845af34596ef37f4754983f85",
"0x388160a99390392278afdba240046b8b5e73f77b",
"0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90",
"0x9f35af4727fb91114563d8a8f779a792cb557f3f",
"0x32ffe815277ff53dd2a73557664e229899e6501e",
"0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6",
"0xbbc37f68e9876d64b2c55016081528ae0a85d8b2",
"0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f",
"0x5de26d392ea46ffc17131042e2584fe6ba46206f",
"0xc97958ff5370c56b7e0e6fb58437f00702714d49",
"0x144c704bf25f1865e4b24fd6596ffed7d92470b0",
"0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e",
"0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56",
"0xadd72e24a9e9117aa16d253cb421cb93b00240e3",
"0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0",
"0x84aeccde4c9f217e83d3fa28c31d34378b903f91",
"0x9b96980c1c018cb617f6653f6892e19ecf4f81e1",
"0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708",
"0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62",
"0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9",
"0xd69e257ae6088b717ae6d2ddec9297703b4fb725",
"0x156bca9cc7b9eebe069182d5302124e4613db9cb",
"0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65",
"0xfd22004806a6846ea67ad883356be810f0428793",
"0xc3c9737cce778d7583f31ae7c4f2ab8782ed51e5",
"0xee05f658e18eb04d250f829b1920c2fbf6907e27",
"0xE16dF6503Acd3c79b6E032f62c61752bEC16eeF2",
"0x9769334FC882775F4951865aA473481880669D47",
"0x3852471D266d9e2222CA9Fdd922BAFC904Dc49e5",
"0x88D3574660711e03196aF8A96f268697590000Fa",
"0x885846850aaBf20d8f8e051f400354D94a32FF55",
"0x61D9d9cc8C3203daB7100eA79ceD77587201C990",
"0xdD6B80649e8D472EB8fb52eb7eEcFd2Dc219AcE7",
"0xE359aB04cEC41AC8C62bc5016C10C749c7De5480",
"0xfe3b3F0D64F354b69A5B40D02f714E69cA4B09bd",
"0x8889EBB11295F456541901f50BCB5f382047cAaC",
"0x0187C9a182736ba18b44eE8134eE438374cf87DC",
"0xbDf82b13580b918ebc3c24b4034E8468EA168E21",
"0xb56ae8a727cf38f1f4716aeda6749d2af340d8f4",
"0x6140F00e4Ff3936702E68744f2b5978885464cbB",
"0x8BA68CFe71550EfC8988D81d040473709B7F9218",
"0xa743c8c57c425B84Cb2eD18C6B9ae3aD21629Cb5",
"0x1b7844CfaE4C823Ac6389855D47106a70c84F067",
"0x76D078D7e5755B66fF50166863329D27F2566b43",
"0xc6400A5584db71e41B0E5dFbdC769b54B91256CD"

)

airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=124)


```

## Airdrop
```{r}
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

## Versioning
```{r}
R.version$version.string
packageVersion("tidyverse")
packageVersion("magrittr")
```
