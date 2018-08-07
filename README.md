# Nivix
Tiny IOT server that accepts HTTP POSTs from a remote solar-powered sensor I built and can serve the information out again in a nice format. Really REALLY minimal.

## Setup details
Just so I don't forget again:
- Build a new VM
- gcloud compute copy-files homepage.html wjw\_hillen@nivix:homepage.html
- gcloud compute copy-files nivixdb.sqlite wjw\_hillen@nivix:nivixdb.sqlite

"Deploy" using the `recompileAndUpload` script, don't forget to restart the server.
