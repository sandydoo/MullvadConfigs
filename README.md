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


Mullvad constantly updates their available servers. If, for whatever reason, you don't use the Mullvad app, your Wireguard server list will go out of date rather quickly. This program downloads the current server list, generates the configuration files, and outputs a ZIP archive.

<br>


#### Disclaimer❗

> **I created this for personal use, without any intention of supporting end-users.** <br>
> **I am releasing this code for educational purposes, especially because it's written in Haskell and nobody writes mundane stuff like this in Haskell.**

<br>


## Usage

You need to have [Stack](stack-url) installed.

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

Run the program using Stack.

```sh
stack run
```


## License

[BSD-3-Clause][license-url] © [Sander Melnikov][maintainer-url].


[wireguard-url]: https://www.wireguard.com/
[mullvad-url]: https://mullvad.net/
[wireguard-config-generator]: https://mullvad.net/en/account/#/wireguard-config/
[stack-url]: https://docs.haskellstack.org/en/stable/README/

[license-url]: https://github.com/sandydoo/MullvadConfigs/blob/main/LICENSE
[maintainer-url]: https://github.com/sandydoo/
