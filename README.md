<div align="center">
  <h1>Mullvad Configs</h1>
  <p>
    <b>
      Generate
      <a href="https://www.wireguard.com/">Wireguard™</a> configurations for
      <a href="https://mullvad.net/">Mullvad</a>.
    </b>
  </p>
  <br>
  <br>
  <br>
</div>


Mullvad’s list of available servers is constantly changing. That’s fine if you’re using their own client, but with a third-party client, like Wireguard’s own clients, it becomes a nuisance. Is my network down or is the server I’m connecting to down for maintenance? Who knows!

This Haskell program downloads the current server list, generates the configuration files, and outputs a ZIP archive.

<br>


#### Disclaimer❗

> **I created this for personal use, without any intention of supporting end-users.** <br>
> **I am releasing this code for educational purposes, specifically because it’s written in Haskell and nobody writes mundane stuff like this in Haskell.**

<br>


## Usage

### Configuration

Create a JSON file called `.peers.json` in the same directory following this format:

```json
[
  {
    "name": "<Device name>",
    "public_key": "<Public key>",
    "private_key": "<Private key>",
    "ipv4_address": "<IPv4 address>",
    "ipv6_address": "<IPv6 address>",
    "ports": [
      "<Open ports>"
    ]
  },
]
```

The IP addresses are specific to your peer. The easiest way to find out yours is by looking at the [configuration generated on the Mullvad website.][wireguard-config-generator]


### Building

There are no pre-built binaries, so you’ll have to build it yourself.

#### Build with Nix

```sh
nix-build release.nix
./result/bin/create-mullvad-configs
```

#### Build with Stack

```sh
stack run
```

#### Build with Cabal

Cabal can’t automatically install some of the external dependencies.

If you’re using this method, make sure to install:
  - zlib
  - bzip2

```sh
cabal run
```


## License

[BSD-3-Clause][license-url] © [Sander Melnikov][maintainer-url].


[wireguard-url]: https://www.wireguard.com/
[mullvad-url]: https://mullvad.net/
[wireguard-config-generator]: https://mullvad.net/en/account/#/wireguard-config/
[stack-url]: https://docs.haskellstack.org/en/stable/README/

[license-url]: https://github.com/sandydoo/MullvadConfigs/blob/main/LICENSE
[maintainer-url]: https://github.com/sandydoo/
