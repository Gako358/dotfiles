# NixOS WSL Config

1.  Configure Nix: If you're using Nix to manage packages in your NixOS WSL environment, you'll need to configure it to use the proxy as well. Create or edit the /etc/nix/nix.conf file (you might need to use sudo to edit this file) and add the following lines, replacing <proxy_address> and <proxy_port> with the correct values:

```
http_proxy = http://<proxy_address>:<proxy_port>
https_proxy = https://<proxy_address>:<proxy_port>
```

### ENVIRONMENT

2. Set the env for systemd

```bash
#!/usr/bin/env bash

proxy_server="www-proxy.address-to-use.com"
proxy_port="8080"

# Create the directory for the Nix daemon service override
mkdir -p /run/systemd/system/nix-daemon.service.d/

# Create the override.conf file with the appropriate proxy settings
cat << EOF >/run/systemd/system/nix-daemon.service.d/override.conf
[Service]
Environment="http_proxy=http://$proxy_server:$proxy_port"
Environment="https_proxy=http://$proxy_server:$proxy_port"
Environment="ftp_proxy=http://$proxy_server:$proxy_port"
EOF

# Reload the systemd daemon and restart the Nix daemon
systemctl daemon-reload
systemctl restart nix-daemon.service

# Print success message
echo "Proxy settings and SSL certificate applied successfully for Nix daemon."
```

### WSL config

3. Edit /etc/wsl.conf

```
generateResolveConf=false
```

### NIX config

4. Edit /etc/nix/nix.conf

Add:

```
ssl-cert-file = /etc/ssl/certs/ca-bundle.crt
```
