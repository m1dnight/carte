# Carte

This is a fun project. It is supposed to mimic the behaviour of
[Fiche](https://github.com/solusipse/fiche).

It is fully functional in the sense that it does what it is supposed
to do. You just pipe some text to a netcat connection and you get back
a link to a paste.

### Client

```
dino :: ~ % echo "Schoolbus" | nc call-cc.be 9999
http://call-cc.be/files/3DFJC
```

### Server

```
./carte -d /var/www/ssl/files -p 9999 -h call-cc.be/files
```

## Contributing

The goal of this project, for me personally, was write someting "real"
in Haskell. I would appreciate comments and issues and whatnot very
highly.
