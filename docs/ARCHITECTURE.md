# Architecture — SawitHeight R v0.4.1

Electron renderer -> preload IPC -> Electron main process -> Rscript -> `r/pipeline.R`.

Backend stages:
1. validation
2. load
3. clean
4. density
5. ground
6. normalize
7. chm
8. tree
9. metrics
10. monitoring
11. residual
12. export
13. complete

The renderer has no direct Node.js access (`nodeIntegration=false`, `contextIsolation=true`, sandbox enabled).
