# Setting up SSL for java dev

Setting up SSL for Java development in WSL with a proxy involves configuring the Java environment to trust the proxy's SSL certificates and configuring the proxy settings for Java. Here's how you can do it:

1. Obtain the proxy SSL certificate: Request the proxy SSL certificate from your network administrator. The certificate should be in PEM format (`.pem`, `.crt`, or `.cer` file). If the certificate is in a different format, you may need to convert it using tools like `openssl`.

2. Import the proxy SSL certificate into the Java truststore: Java uses a truststore (a file named cacerts) to manage trusted certificates. To import the proxy SSL certificate into the Java truststore, you can use the `keytool` command that comes with the JD

```
sudo keytool -import -alias <proxy_cert_alias> -file /path/to/proxy_cert.pem -keystore /path/to/java/jdk/lib/security/cacerts -storepass changeit
```

Replace `<proxy_cert_alias>` with a unique alias for the proxy certificate, `/path/to/proxy_cert.pem` with the path to the proxy SSL certificate file, and `/path/to/java/jdk/lib/security/cacerts` with the path to the cacerts file in your Java installation. The default password for the Java truststore is `changeit`.

1. Configure proxy settings for Java: Set the `http.proxyHost`, `http.proxyPort`, `https.proxyHost`, and `https.proxyPort` system properties to configure the proxy settings for Java. You can do this by modifying the `JAVA_OPTS` environment variable in your .bashrc file or any other shell configuration file you use:

```
export JAVA_OPTS="$JAVA_OPTS -Dhttp.proxyHost=<proxy_address> -Dhttp.proxyPort=<proxy_port> -Dhttps.proxyHost=<proxy_address> -Dhttps.proxyPort=<proxy_port>"
```

Replace `<proxy_address>` and `<proxy_port>` with your proxy server's address and port.

1. Save the changes and restart the terminal.

## Using a flake

```
{
  description = "A Java development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      rec {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.jdk11
            pkgs.maven
          ];
          
          shellHook = ''
            export JAVA_OPTS="-Dhttp.proxyHost=<proxy_address> -Dhttp.proxyPort=<proxy_port> -Dhttps.proxyHost=<proxy_address> -Dhttps.proxyPort=<proxy_port>"
          '';
        };
      }
    );
}
```

After running `nix devel`
```
echo $JAVA_HOME/lib/security/cacerts
```

Then import the proxy SSL certificate into the java truststore:

```
sudo keytool -import -alias <proxy_cert_alias> -file /path/to/proxy_cert.pem -keystore $JAVA_HOME/lib/security/cacerts -storepass changeit
```

Replace `<proxy_cert_alias>` with a unique alias for the proxy certificate, `/path/to/proxy_cert.pem` with the path to the proxy SSL certificate file, and `/path/to/java/jdk/lib/security/cacerts` with the path to the cacerts file in your Java installation. The default password for the Java truststore is `changeit`.
