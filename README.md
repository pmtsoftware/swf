# deploy instructions

```
scp ~/.local/bin/swf-exe linuxuser@70.34.246.215:pnl
rsync -av ./static linuxuser@70.34.246.215:~/pnl
rsync -av ./migrations linuxuser@70.34.246.215:~/pnl
```
