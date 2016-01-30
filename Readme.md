
# What

A CHICKEN egg for creating HTTP-servers. Inspired by Clojure's ring.

## Dependencies



# Example

Run `csi -s example.scm` and try it:

```sh
$ curl localhost:8080/
root
$ curl localhost:8080/echo
no matching uri for (GET echo)
$ curl localhost:8080/echo -d 'hi'
hi
$ curl localhost:8080/bad
no matching uri for (GET bad)
```

If you edit the example to use nrepl, so you can also do:

```sh
echo '(define (app r) (response body: "you have been hacked"))' | nc localhost 1234
```

Which yields:


```
curl localhost:8090/anything -d "POST to root"
you have been hacked
```

This is very useful because it let's you redefine your server's
behaviour and REST-APIs while it's running.

# TODO

- make a json wrapper to show how easy life can be
- support better logging
- unwrap uri-path in request map? 
- user-code must (use matchable) because of our syntax, I think. this is bad
