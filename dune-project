(lang dune 3.17)

(name nyse-sim)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/nyse-sim))

(package
 (name nyse-sim)
 (synopsis
  "NYSE Pillar gateway and XDP integrated feed simulator over the exchange-core engine")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  cohttp-async
  http
  async_websocket
  uri
  yojson
  fixed-decimal
  exchange-core
  exchange-core-engine
  order-book
  equities-core
  market-data
  price-anchor
  ppx_jane))
