# Searching for i-Good Lemmas to Accelerate Safety Model Checking 阅读报告

## 1. 论文主题与核心问题

这篇论文关注的是一个很具体但很重要的问题：IC3/PDR 和 CAR 在安全性证明过程中，会不断生成新的 lemma 来加强 frame sequence，而 **不同 lemma 的“质量”差异很大**。有些 lemma 很容易继续前推，帮助算法更快到达 fixpoint；有些 lemma 虽然当前合法，但对后续收敛帮助很小。

论文提出的核心观点是：

- 与其泛泛地“找更小的 generalized clause”，不如有意识地引导搜索，尽量生成更容易前推的 lemma；
- 为此，引入了 **i-good lemma** 的概念；
- 并在此基础上提出两个轻量级启发式：
  - `branching`
  - `refer-skipping`

这两个启发式都不改变 IC3/CAR 的正确性，只影响搜索路径和 lemma 的形状。

---

## 2. i-good lemma 的含义

论文定义：

- 一个 lemma `c` 在 frame `i` 上成立；
- 如果 `c` 在 frame `i+1` 上也成立，则称 `c` 为 **i-good lemma**。

直觉上：

- 这种 lemma 已经不只是“当前层可用”；
- 它还能继续前推；
- 因而更接近最终不变式中的 clause；
- 如果搜索过程中能生成更多 i-good lemma，算法通常更容易收敛。

论文并不试图只生成 i-good lemma，因为那本身几乎和求解问题一样难。它真正做的是：

- 用启发式提高“生成 i-good lemma 的概率”。

---

## 3. 论文的两个关键技术

论文真正的关键技术只有两个：

1. `branching`
2. `refer-skipping`

另外，`i-good lemma` 是这两个技术的基础概念。

### 3.1 branching

`branching` 的基本想法是：

- 维护一个变量分数表 `S[v]`；
- 分数表示“变量 `v` 最近多频繁地出现在 i-good lemma 中”；
- 在 SAT 查询和 generalization 的排序中使用这个分数。

论文借鉴了 CDCL SAT solver 中类似 VSIDS 的思想，但没有修改 SAT solver 内部，而是通过 **对 assumption literals 排序** 来间接影响 SAT 搜索和 unsat core/predecessor 的形状。

### 3.2 refer-skipping

`refer-skipping` 的基本想法是：

- 在 generalization 当前 lemma `c` 时，如果前一层已经存在某个子句 `g`，满足 `g ⊆ c`；
- 那么将 `g` 作为 reference；
- generalization 时不再尝试删除 `g` 中的 literal；
- 从而让 generalized 结果更接近前一层已有结构，更可能成为 `(i-1)-good lemma`。

它本质上是一个 **利用前一层已有结构约束当前泛化方向** 的机制。

---

## 4. branching 的具体实现

这是论文里最值得作为 baseline 先实现的部分。

### 4.1 分数对象

论文维护的是一个 **变量级分数表**：

```text
S[v]
```

注意：

- 分数是给变量，不是给 literal，也不是给 clause。
- 所以同一变量上的不同 literal 共用一个分数。

### 4.2 reward 的触发条件

论文不是对所有 lemma 奖励，只对被视为 i-good 的 lemma 奖励。

主要有两种触发方式：

1. **propagation 成功**
   - 某个 lemma `c` 能从 frame `i` 推到 frame `i+1`；
   - 则它是 i-good；
   - 奖励 `c` 中所有变量。

2. **generalization 得到的结果和前一层结构一致**
   - 在 frame `i+1` 中，将一个更大的 clause `d` generalized 成更小的 clause `c`，其中 `d ⊇ c`；
   - 若 `c` 在 frame `i` 已经有一个“父节点”/参考结构可对应；
   - 也将 `c` 视为一种 i-good 候选并奖励。

第二种情况依赖 `get parentnode(c)`。

### 4.3 reward 的更新规则

论文的 reward 实现很简单：

1. 先对所有变量分数做衰减：

```text
S[v] := 0.99 * S[v]
```

2. 再对 lemma `c` 中出现的每个变量加分：

```text
S[v] := S[v] + 1
```

论文实现里：

- 衰减系数是 `0.99`
- 增量是 `1`

这使得分数更接近“近期活跃度”，而不是“历史累计总次数”。

### 4.4 为什么要衰减

如果没有衰减：

- 早期频繁出现过的变量会长期占据高分；
- 即使后续已经不重要，也很难被新变量超越。

引入衰减后：

- 最近还经常出现在 i-good lemma 中的变量会保持高分；
- 很久没出现的变量分数会逐渐下降；
- 搜索方向会随着当前阶段变化而自适应变化。

### 4.5 分数具体用在哪里

论文把 `S[v]` 用在三个地方。

#### 4.5.1 SAT assumptions 排序：降序

在每次 SAT 查询之前，如果 assumptions 中包含某个 lemma `c`（或 next-state 版本 `c'`）对应的 literals，那么：

- 先按 `S[var(l)]` 对 literals 排序；
- 排序方式是 **降序**。

即：

- 高分变量的 literal 优先进入 assumptions。

论文提到的使用位置有：

- `Unsafecheck`
- `Get predecessor`
- `Generalization` 中的 CTG 相关 SAT 查询

目的：

- 让 SAT solver 更偏向围绕“高分变量”工作；
- 使 unsat core、predecessor、counterexample cube 更可能包含这些变量；
- 最终提高 i-good lemma 的出现概率。

#### 4.5.2 inductive generalization 中 literal 排序：升序

在 generalization 阶段，论文不是按原顺序尝试删 literal，而是：

- 将 clause `c` 中的 literal 按 `S[var(l)]` **升序**排序；
- 低分变量的 literal 先尝试删；
- 高分变量的 literal 后尝试删。

原因是：

- 高分变量更可能属于 i-good lemma 的关键结构；
- 因而应尽量保留；
- 低分变量则更适合先试着删除。

也就是：

- SAT assumptions 用 **降序**
- generalization 删 literal 用 **升序**

这是论文最值得注意的一个实现细节。

#### 4.5.3 `get parentnode(c)` 的多候选选择

如果前一层有多个可作为 parent/reference 的候选，论文选择：

- 分数最高的那个。

这里的“分数最高”本质上还是偏向包含更多高分变量的 reference。

---

## 5. refer-skipping 的具体实现

这是论文第二个关键技术。

### 5.1 它想解决的问题

对于同一个 clause `c`，generalization 可能得到多个不同子句：

- `g1 ⊆ c`
- `g2 ⊆ c`

它们都合法，但强弱未必可比，且对后续前推的帮助不同。

论文希望：

- 当前 generalized 结果尽量向前一层已有“好结构”靠拢；
- 而不是任意漂移到别的泛化方向。

### 5.2 reference 的定义

如果在 frame `i-1` 已经存在某个 lemma `g`，满足：

```text
g ⊆ c
```

那么 `g` 可以作为当前 clause `c` 的一个 reference。

论文认为：

- 这样的 `g` 是当前 generalization 的一个良好方向；
- 如果 generalized 结果尽量保留 `g` 的结构，就更有希望成为 `(i-1)-good lemma`。

### 5.3 refer-skipping 的做法

给定当前要 generalized 的 clause `c`：

1. 调用 `get parentnode(c)`；
2. 在前一层找一个 reference `p`，满足 `p ⊆ c`；
3. 将 `p` 中出现的变量放入一个集合 `req`；
4. generalization 时，只尝试删除那些 **不在 `req` 中** 的 literal；
5. `req` 中的 literal 直接跳过，不尝试删除。

也就是说：

- reference 中的 literal 被“黑名单保护”；
- generalization 不会再试图删掉它们；
- 当前 generalized 结果被硬性引导向 reference 靠拢。

### 5.4 论文里的例子

当前层 clause：

```text
c = ¬1 ∨ 2 ∨ ¬3
```

前一层已有 clause：

```text
g = ¬1 ∨ ¬3
```

因为 `g ⊆ c`，所以拿 `g` 作 reference。

那么 generalization 时：

- `¬1` 不删
- `¬3` 不删
- 只尝试是否删除 `2`

这样 generalized 结果自然会向 `g` 靠近。

### 5.5 `req` 的作用

论文里的 `req` 原本就用于记录：

- 那些已经尝试删除但失败的 literal；
- 后续就不再重复尝试删除它们。

`refer-skipping` 的实现只是复用这个机制：

- 初始化时，先把 reference 中的 literal 对应变量装进 `req`；
- 相当于一开始就把这些 literal 标记为“必须保留”。

因此它实现起来很轻量。

### 5.6 refer-skipping 的性质

- 它不改变算法正确性；
- 它只是减少 generalization 搜索空间；
- 并把 generalized clause 硬性拉向前一层已有结构。

与 `branching` 相比：

- `branching` 是软引导；
- `refer-skipping` 是硬约束。

---

## 6. 实验部分

### 6.1 实验目标

论文实验部分主要想回答两个问题：

1. `branching` 和 `refer-skipping` 是否真的能提升 IC3/CAR 的整体性能；
2. 如果性能提升存在，是否和“生成更多 i-good lemma”之间存在关系。

因此，实验不是只看最终 solved 数，还额外测量了生成 i-good lemma 的成功率。

### 6.2 实验对象

论文将这两个启发式集成到了 3 个系统中：

- `IC3Ref`，对应 IC3
- `SimpleCAR`，对应 Forward CAR
- `nuXmv` 中的 IC3 实现

同时还引入了几个外部对照系统：

- `IIMC` 中的 IC3
- `IIMC` 中的 QUIP
- `ABC` 中的 PDR

其中，`QUIP` 被纳入实验，是因为论文方法和 QUIP 都关注“更好的 lemma”，但论文指出 QUIP 的公开实现性能并不理想，且其工程实现难度较高。

### 6.3 配置设计

对于论文自己修改过的几个系统，实验配置包括：

- `-br`：只启用 branching
- `-rs`：只启用 refer-skipping
- `-br -rs`：同时启用 branching 和 refer-skipping
- `-sh`：将原本的 sort() 替换为随机 shuffle

其中 `-sh` 是一个非常重要的对照：

- 它用来验证“性能改善是否只是因为改了顺序”；
- 如果随机排序也同样有效，那论文的启发式意义就会下降；
- 实验结果表明随机排序没有呈现稳定提升，因此论文认为提升来自 i-good lemma 导向的排序，而不是任意打乱顺序。

### 6.4 benchmark 和运行环境

实验使用了：

- HWMCC 2015 与 2017
- SINGLE safety property track
- AIGER 格式 benchmark
- 共 749 个实例

运行环境为集群：

- 每次运行独占一个节点
- 内存限制 8GB
- 时间限制 5 小时

正确性方面，论文还做了额外检查：

- 各 solver 的结果互相比对，确认无冲突；
- unsafe 情况下，用 `aigsim` 检查 counterexample；
- 最终没有发现错误结果。

### 6.5 总体性能评估方式

论文首先统计每个配置：

- 总共 solved 多少 benchmark
- solved 的 safe benchmark 数
- solved 的 unsafe benchmark 数
- 相对 baseline 的 gain
- 相对 baseline 的 loss

这里的 baseline 指：

- 同一个工具中，不启用 `-br` 和 `-rs` 的版本。

换言之，论文在每个 solver 内部做同平台对比，而不是混在不同工具之间直接比。

### 6.6 总体实验结论

论文从 Table 4 和 Fig. 1 中总结出以下主要结论：

- 开启至少一个启发式时，整体上都优于对应 baseline；
- 对 safe benchmark 的改善尤其明显；
- 对 unsafe benchmark 也有提升；
- `branching + refer-skipping` 通常优于单独启用其中一个；
- 但 gain/loss 并不均匀，说明这是启发式，而不是单调占优的改进；
- 随机排序 `-sh` 没有明显稳定收益；
- 与 QUIP 和 ABC 等系统相比，论文的方法在实现复杂度较低的前提下表现出了较强竞争力。

论文也特别提到：

- QUIP 的想法虽然相关，但其公开实现效果较差；
- 这反过来说明，像 branching 和 refer-skipping 这样轻量、可集成的启发式在工程上更实用。

### 6.7 可视化分析

论文使用了两类主要图表。

#### 6.7.1 Cumulative solved-time 曲线

Fig. 1 展示的是：

- 横轴：CPU 时间
- 纵轴：累计 solved benchmark 数

这个图用于说明：

- 启发式版本能否在同样时间内解出更多 benchmark；
- 以及不同工具之间的总体趋势。

#### 6.7.2 启发式 vs baseline 的时间散点图

Fig. 2 用散点图比较：

- 启发式版本时间
- baseline 时间

点落在对角线一侧表示启发式更快。

论文通过这张图说明：

- `-br -rs` 在大量 benchmark 上确实降低了时间；
- 尤其对 `IC3Ref` 和 `SimpleCAR` 比较明显；
- 单独启用一个启发式时，波动更大。

### 6.8 为什么这两个启发式有效：机制性指标

论文实验中最重要的一点，不只是报告 solved/time，而是试图解释“为什么有效”。

为此，他们定义了一个指标：

```text
sr = Ng / N
```

其中：

- `N`：generalization 总调用次数
- `Ng`：成功返回 i-good lemma 的 generalization 次数

也就是：

- `sr` = 生成 i-good lemma 的成功率

这是论文从性能分析走向机制解释的关键步骤。

### 6.9 `sr` 是怎么测的

论文给两个开源实现加了 instrumentation：

- `IC3Ref`
- `SimpleCAR`

记录每次运行过程中：

- 一共做了多少次 generalization
- 其中多少次 generalization 产生了 i-good lemma

然后比较 baseline 和启发式版本的 `sr`。

### 6.10 `sr` 分析得到的结论

Fig. 3 比较了：

- baseline 的 `sr`
- 开启 `branching + refer-skipping` 后的 `sr`

结果表明：

- `ic3 -br -rs` 在 54% 的测试实例上，`sr` 更高；
- `fcar -br -rs` 在 67% 的测试实例上，`sr` 更高。

论文据此认为：

- 启发式确实提高了生成 i-good lemma 的概率。

### 6.11 `sr` 与性能之间的关系

Fig. 4 则进一步比较：

- `sr` 的变化量
- CPU 时间的变化量

它想验证：

- “更多 i-good lemma”是否与“更好的模型检测性能”存在相关性。

实验结果总体支持这个猜想：

- 当启发式让 `sr` 上升时，运行时间往往会下降；
- 因此论文认为：引导搜索生成更多 i-good lemma，是提升 IC3/CAR 性能的有效方向。

### 6.12 对我们后续实验设计的启发

论文实验设计最值得借鉴的地方在于：

- 不只看最终 solved 数；
- 还设计了一个机制性指标 `sr`；
- 并通过随机排序 `-sh` 做消融对照；
- 尝试建立“启发式 -> 更多 i-good lemma -> 更好性能”的链条。

因此，如果我们后续要在当前 IC3 上复现或扩展论文方法，一个合理的实验结构应该包括：

1. 原始 baseline
2. 论文 baseline：`-br`
3. 论文 baseline：`-rs`
4. 论文 baseline：`-br -rs`
5. 随机排序对照
6. 我们自己的增强版本
7. 额外记录：
   - generalization 次数
   - i-good lemma 次数
   - `sr`

这样做，实验结果才会既有性能层面的说服力，也有机制层面的解释力。

---

## 7. `get parentnode(c)` 的作用

论文中 `get parentnode(c)` 有两个用途：

1. 判断 generalization 得到的结果是否值得奖励；
2. 为 `refer-skipping` 提供 reference。

它返回的是前一层中的某个 cube/lemma `p`，满足：

```text
p ⊆ c
```

如果有多个候选：

- 论文选择“分数最高”的那个。

直观理解：

- `get parentnode(c)` 负责在上一层里找到最合适的“父结构”；
- 再用这个父结构指导当前 generalization 的方向。

---

## 8. 论文方法的整体闭环

论文的方法不是几个孤立技巧，而是一个闭环：

1. 搜索过程中产生 clause；
2. 若 clause 能前推或与前一层结构一致，则视为 i-good；
3. 奖励其中变量，更新 `S[v]`；
4. 在后续 SAT queries 中，优先考虑高分变量；
5. 在 generalization 中，优先删除低分变量；
6. 在 refer-skipping 中，再用上一层 reference 硬性约束泛化方向；
7. 从而更容易继续产生新的 i-good lemma。

这就是论文所谓“steer the search towards i-good lemmas”的具体含义。

---

## 9. 对我们当前实现的启发

从实现角度看，论文 baseline 的最小核心是：

1. 维护变量分数 `S[v]`
2. `reward(c)`：衰减 + 奖励
3. assumptions 排序：按分数降序
4. `ind_generalize` 排序：按分数升序
5. `get parentnode` + `refer-skipping`

这五点就构成了论文最核心的启发式实现。

需要注意的是，论文的权重设计并不复杂：

- 它没有做 literal 级别学习；
- 没有做 clause-level 模板学习；
- 没有做 keep 后动态重排；
- 没有利用“变量簇”或“共享变量数”等更细粒度信息。

因此它很适合作为当前 IC3 实验中的一个 baseline。

---

## 10. 我们讨论后的理解总结

结合本次讨论，可以将论文理解为：

- 它并不是在重新发明一种复杂 generalization 算法；
- 而是在现有 IC3/CAR 框架中，加上一套很轻量的排序/保留启发式；
- 核心信号是“哪些变量最近常出现在 i-good lemma 中”；
- 再配合“上一层已有结构尽量别删”的 refer-skipping；
- 从而让搜索更偏向那些更容易继续前推的 lemma。

从工程实现上看，这篇论文的价值主要在于：

- 提供了一个结构清晰、实现开销低的 baseline；
- 为后续更复杂的启发式提供了一个规范起点。

---

## 11. 我们后续可在此基础上改进的方向

在论文基础上，可以继续做的改进包括：

- 从变量分数升级到 literal 分数；
- 增加模板分数，例如 `x = c`、`x >= c`、`x != c`；
- 在 `ind_generalize` 中做 keep 后动态重排；
- 根据 `kept` 中变量，优先尝试共享变量更多的剩余 literals；
- 将当前 clause 的局部结构和历史分数结合。

但这些改进都应建立在一个前提上：

- 先在当前 IC3 上实现论文原始 baseline；
- 再做后续增强；
- 这样实验对比才清晰可靠。

---

## 12. 最终结论

这篇论文的主要贡献可以压缩为一句话：

> 用变量历史分数和前一层参考结构，引导 IC3/CAR 更倾向于生成能够继续前推的 lemma。

其两个关键实现点分别是：

- `branching`：高分变量在 SAT 查询中优先，在 generalization 中尽量晚删；
- `refer-skipping`：前一层 reference 中的 literal 不再尝试删除。

论文方法轻量、易于集成，非常适合作为我们当前工作中的 baseline。
