# ST458 Group Project — GBM Based Trading Strategy

## 怎么用

超简单，三步：

1. 把整个 repo clone 下来（或者整个zip下载下来），保持文件夹结构别动
2. 在 RStudio 里打开 `Report Template.Rmd`
3. 点 **Knit** (或者 `Knit to PDF`)，会自动生成 `Report-Template.pdf`

### 注意事项

- 有几个 chunk 设了 `cache=TRUE`，第一次 knit 会稍微慢一点 (主要是训练 LightGBM 和跑 walk-forward backtest)，之后就快了。
- 超参数调优的 chunk 默认 `eval=FALSE`，直接读 `./Results/hp_tuning_results.rds`，不会重跑网格搜索。要重跑的话把那个 chunk 的 `eval=FALSE` 去掉就行。

---

## 文件夹说明

| 路径 | 内容 |
|------|------|
| `Report Template.Rmd` | 报告主文件，直接 knit 这个就行 |
| `Report-Template.pdf` | knit 出来的成品 PDF，参考用 |
| `Data/` | 训练数据 `df_train.csv`，100 只合成 ETF 的 open/close/volume |
| `Strategy/` | 三个策略脚本：`GroupA.R` (我们的 LightGBM 策略，主角)、`Real.R` (momentum benchmark)、`Equal.R` (equal-weight 基准) |
| `Test/` | `walk_forward.R`，回测框架，Rmd 里会 source 它 |
| `Results/` | 缓存的超参数调优结果 (`hp_tuning_results.rds`)，避免每次 knit 都重跑网格搜索 |
