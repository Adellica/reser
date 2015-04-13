
# What

A CHICKEN egg for creating HTTP-servers. Inspired by Clojure's ring.

# Example

Run `csi -s example.scm` and try it:

```sh
curl localhost:8080
usage: POST some data to /echo

~ $ curl localhost:8080/echo -d "foobar"
foobar right back atcha

~ $ curl localhost:8080/images
your toplevel path is images

~ $ curl localhost:8080/ -d "POST to root" -D -
HTTP/1.1 404 Not Found
Content-Type: text/html
Server: Spiffy/5.4 (Running on Chicken 4.9.1)
Content-Length: 15
Date: Mon, 13 Apr 2015 13:29:35 GMT

url not found!
```

The example runs nrepl in the background, so you can also do:

```sh
echo '(define (app r) (response body: "you have been hacked"))' | nc localhost 8081
```

Which yields:


```
curl localhost:8090/anything -d "POST to root"
you have been hacked
```

This is very useful because it let's you redefine your server's
behaviour and REST-APIs while it's running.

# TODO

- depend on persistent-hash-map? can we just use alists instead?
- make a json wrapper to show how easy life can be
- support better logging
- unwrap uri-path in request map? 
- user-code must (use matchable) because of our syntax, I think. this is bad
