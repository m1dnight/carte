# Carte

This is a fun project. It is supposed to mimic the behaviour of
[Fiche](https://github.com/solusipse/fiche).

It is fully functional in the sense that it does what it is supposed
to do. You just pipe some text to a netcat connection and you get back
a link to a paste.

## Building

The usual.

```
git clone https://github.com/m1dnight/carte
cd carte
cabal update
cabal sandbox init
cabal configure
cabal install
cabal build
```

## Example Usage

### Client

```
$ echo "Schoolbus" | nc call-cc.be 9999
https://call-cc.be/files/3DFJC
```

### Server

```
./carte -d /var/www/ssl/files -p 9999 -u https://call-cc.be/files
```

## Contributing

The goal of this project, for me personally, was write someting "real"
in Haskell. I would appreciate comments and issues and whatnot very
highly.


## Deploying

Here's how I deployed the application on my Debian Jessie server.

Create user
```
sudo addgroup --system carte
sudo adduser --disabled-password --system --ingroup carte carte
sudo git clone https://github.com/m1dnight/carte /opt/carte
sudo chown -R carte:carte /opt/carte
sudo usermod -a -G www-data carte
sudo chmod -R 775 /opt/carte
sudo chmod -R 775 /var/www/ssl/pastes
```

Compile
```
su carte
cd /opt/carte
cabal update
cabal sandbox init
cabal configure
cabal install
cabal build
```

Configure SystemD
```
cp /opt/carte/init/carte.service /etc/systemd/system/carte.service
systemctl enable /etc/systemd/system/carte.service
sudo systemctl start carte.service
```

SystemD file
```
[Service]
WorkingDirectory=/opt/carte/dist/build/carte
ExecStart=/opt/carte/dist/build/carte/carte -p "9999" -u "https://call-cc.be/pastes" -d "/var/www/ssl/pastes"
Restart=always
StandardOutput=syslog
StandardError=syslog
SyslogIdentifier=carte
User=carte
Group=carte

[Install]
WantedBy=multi-user.target

[Unit]
Wants=network-online.target
After=network.target network-online.target
```
