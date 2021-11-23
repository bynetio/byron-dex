Running
=======

```
> cabal run faucet-app --help

Faucet App

Usage: faucet-app (start | showconfig)

Available options:
  -h,--help                Show this help text

Available commands:
  start                    
  showconfig
```

```
> cabal run faucet-app -- start --config app.config
```

API
===

List of endpoints:
* http://localhost:8085/faucet/{address} - request a given token (using token name) on a given address
* http://localhost:8085/tokens           - list of available tokens


### geting list of all available tokens to request

```
curl http://localhost:8085/tokens | jq
```

```
[
  {
    "tokenName": "coinA",
    "tokenCurrency": "8aec86a2545cd7e1ea189c29a6e87ed0ead3ab19404ec1603ee6aa05"
  },
  {
    "tokenName": "coinB",
    "tokenCurrency": "8aec86a2545cd7e1ea189c29a6e87ed0ead3ab19404ec1603ee6aa05"
  },
  {
    "tokenName": "coinC",
    "tokenCurrency": "8aec86a2545cd7e1ea189c29a6e87ed0ead3ab19404ec1603ee6aa05"
  }
]

```

## requesting a token

```
 curl -v -X POST \
   -H "Content-Type: application/json" \
   -d '"coinC"' \
   http://localhost:8085/faucet/addr_test1qpchh8n80lfg7x60j3k3ww2kcmsct2dr7hv3r78wxtvhkls3gjjjywj4la9jur9wxfq85qr6ycmmde8tetsdsw076pwstr7ldl
```

In case of success a transaction id is returned
```
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Mon, 15 Nov 2021 20:55:47 GMT
< Server: Warp/3.3.17
< Content-Type: application/json;charset=utf-8
"02d45a18a74e3bd76c6aeba960c47e11ce596347f09fd354b8034ec5da65a552"
```

### In case of error

There are following type of errors
* Internal Server error (500)
* Token not supported (404)
* No Utxo to consume (409) - could appear in case of high request volume, normally waiting a couple of seconds and repeating request should be enough

```
< HTTP/1.1 404 Not Found
< Transfer-Encoding: chunked
< Date: Mon, 15 Nov 2021 20:54:58 GMT
< Server: Warp/3.3.17
< Content-Type: application/json
"Token not supported: 'coinCC'"
```

```
< HTTP/1.1 409 Conflict
< Transfer-Encoding: chunked
< Date: Mon, 15 Nov 2021 21:04:39 GMT
< Server: Warp/3.3.17
< Content-Type: application/json
"Wait few seconds and try again"
```

Setup
=====

Config is divided into following sections
* server
* faucet   - configuration of the faucet
  - mint   - defines what kind of tokens (and quantity) a user can request
  - wallet - defines a source of test ada, faucet is using to mint the tokens
* node     - defines how to connect to the running cardano node

Sample config could look following

```
{ 
  server = { 
    port = 8085, 
    host = "0.0.0.0" 
  },
  faucet = {
    mint = {
      tokens = ["coinA", "coinB", "coinC"],
      quantity = 1002,
    },
    wallet = {
        address = "addr_test1qrexdmlktfa55uddpm0s79yfaht9c77h0qm0nd224hndqaea055p66w7gjp65zusx40xfl6q9eng4x6l3vxg9rhf5nrs08c6r2",
        skey = "/tmp/9c70aac3-1445-4b67-9d70-39cbd4bbb347/payment.skey",
        lovelace = 1500000
    }
  },
  node = {
    magic = "1097911063",
    network = "Testnet",
    socket = {
      path = "/tmp/.cardano-node/node-ipc/socket"
    }
  }
}
```

## defining test ada source

Each Utxo sitting on the wallet address is treated as a source of test ada. 
The number of Utxo defines a faucet paralellism. The more sources the more transactions can be handled per block.

In order to make such source one need to make a pay to `faucet.wallet.address` address.

Such pay can be performed using [byron-cardano-toolkit/cli-toolbox](https://github.com/byron-network/byron-cardano-toolkit/tree/main/cli-toolbox)

create faucet wallet
```
./wallet.sh -c faucet
```

list all available wallets
```
./wallet.sh -l
Id                                      Name            Desc
==============================================================
148cfcf4-6003-4179-9247-00ae714ae9e1    root
9c70aac3-1445-4b67-9d70-39cbd4bbb347    faucet
```

fund faucet with 4 Utxo (level of parallelis = 4)
```
./pay-to-wallet.sh -w root -d $(./wallet.sh -a faucet) -v 100000000
./pay-to-wallet.sh -w root -d $(./wallet.sh -a faucet) -v 100000000
./pay-to-wallet.sh -w root -d $(./wallet.sh -a faucet) -v 100000000
./pay-to-wallet.sh -w root -d $(./wallet.sh -a faucet) -v 100000000
```

check the funds on a faucet wallet
```
./query-utxo.sh -w faucet

Quering utxo at addr_test1qrexdmlktf...l3vxg9rhf5nrs08c6r2 (node sync: 100.00) ('q' for quit, enter for query again)

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
96625ad9f6edff73c8155372c1fc34b19a98bced306a68f634008e6e48bd46b2     0        79698378 lovelace + TxOutDatumHashNone
972cbed3863fdf57f903362da5557e315b259d5fb2a1d089dd7ba4a02588693b     0        88925812 lovelace + TxOutDatumHashNone
b4dfce60edcf7d3cea968087bcca04aef6b32536ad56937c25127f474d9fc80b     0        88925812 lovelace + TxOutDatumHashNone
ec47e36aa9ce315b06dca799d9905c0419491025060a3ea882d00ce83ff28735     0        77851448 lovelace + TxOutDatumHashNone
```


