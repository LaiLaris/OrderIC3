# Triggered Clause Pushing for IC3

## 论文信息

- 标题: *Triggered Clause Pushing for IC3*
- 作者: Martin Suda
- 时间: 2013

## 一句话总结

这篇论文给 IC3 的 clause propagation / clause pushing 阶段加入了一个“按需触发”的机制：当某个子句之前推不动时，不再简单丢弃 SAT 求解器返回的模型，而是把它保存成一个 witness；以后只有当新学到的子句把这个 witness 排除掉时，才重新尝试 pushing。这样可以减少大量无意义的 SAT 调用，并让子句始终尽可能被推到更深的 frame。

## 为什么要做

IC3 在证明安全性质时，会维护一串 layers / frames:

- `L0 = I`
- `L1, L2, ...` 是对可达状态的逐层过近似

IC3 的一个关键步骤是 **clause propagation**，也就是检查某个子句 `c` 是否可以从 `L_i` 推到 `L_{i+1}`。如果能推，说明这个子句在更深一层依然成立，这有几个直接好处：

- layers 会更强，后续阻塞状态时上下文更有力
- 更容易更早收敛到归纳不变式
- 更快发现相邻两层相等，即 `L_i = L_{i+1}`

问题在于，标准 IC3 中 propagation 通常是“成批做”的。对于一个推不动的子句，求解器会返回 SAT 和一个模型，但这个模型一般被直接丢掉。这样会导致两个低效点：

- 不知道什么时候值得重新尝试这个子句
- 如果频繁重试，每次都要重新做一次 SAT 调用，代价高

论文的出发点就是：**失败的 pushing 其实已经告诉了我们“为什么推不动”，这个信息应该被保留下来。**

## 标准 IC3 中 pushing 的含义

对于子句 `c ∈ L_i \\ L_{i+1}`，IC3 会检查：

`L_i ∧ T ⇒ c'`

通常转成 SAT 查询：

`L_i ∧ T ∧ ¬c'`

- 如果 UNSAT，说明 `c` 可以被 push 到 `L_{i+1}`
- 如果 SAT，说明存在一个状态 `w`，它满足 `L_i`，并且可以一步转移到一个违反 `c` 的后继状态，因此 `c` 当前不能被 push

标准做法里，这个 SAT 模型用完就丢。

## 论文的核心观察

论文把 pushing 失败时得到的状态 `w` 保留下来，记作子句 `c` 的 **witness**。

这个 witness 的作用是解释：

- 为什么 `c` 目前不能从 `L_i` 推到 `L_{i+1}`

只要这个 witness 仍然满足当前的 `L_i`，那么上面的 SAT 查询就仍然可满足，`c` 就仍然推不过去。因此：

- **没有必要反复重试 pushing**

只有当 `L_i` 之后被新子句加强，导致这个 witness 不再满足 `L_i` 时，才值得重新尝试。

这就是 Triggered Clause Pushing 的核心思想：

- 失败时保存 witness
- 仅在 witness 被新子句“杀掉”时重试 pushing

## 怎么判断 witness 失效

论文的关键技巧是把这个判断转成一个便宜的 **subsumption** 检查。

设：

- `w_c` 是 clause `c` 的 witness，对应一个 state cube
- `d` 是后来新加入 `L_i` 的 clause

如果 `d` subsumes `¬w_c`，那么 `w_c` 就不再满足强化后的 `L_i`，因此它不再能作为 `c` 不能 push 的证据。此时就应该：

- 重新对 `c` 发起 pushing 查询

所以，论文把“何时重试”转成了：

- 当且仅当新子句 subsume 掉 witness 时触发

这个条件既便宜，又足够精确。

## 具体怎么做

### 1. pushing 失败时保存 witness

对每个未能 push 的子句，保存一个 witness，而不是把 SAT 模型丢掉。

### 2. 新子句加入时做 subsumption

每当 IC3 学到一个新子句并加入某层时，除了做普通的 clause subsumption，还额外检查：

- 它是否 subsume 掉某些 witness

如果是，就说明这些 witness 失效了，于是把对应子句加入待 push 集合。

### 3. 把 propagation 融进主循环

标准 IC3 里 propagation 常常在每轮结束时统一执行一次。论文改成：

- 维护待处理的 push requests
- 一旦 witness 失效，就把对应 clause 标记为应当重试 pushing

这样 clause propagation 不再是偶尔集中做一次，而是随着搜索过程持续发生。结果是：

- 子句总是尽量被推到当前能到的最远层
- layers 始终更强
- 更早检测到收敛

## 数据结构组织

论文为了把这件事高效实现，重新组织了 IC3 的几个集合。

### Delta layers

它采用 delta 编码：

- `Δ_i = L_i \\ L_{i+1}`

这样只在某一层“首次出现”的子句存一次，避免重复存储。

并且有：

- `L_i = ⋃_{j ≥ i} Δ_j`

### 三类对象

每个层级 `i` 维护：

- `O_i`: proof obligations
- `R_i`: push requests
- `W_i`: witnesses

此外，每个 clause 要么：

- 关联一个 witness，表示“现在还推不动”
- 要么关联一个 push request，表示“现在该尝试 push”

### 新子句加入时的联动处理

当一个新 clause `c` 被加入 `Δ_i` 时，系统会做几件事：

1. 把 `c` 对应的 pushing 请求加入 `R_i`
2. 删除 `Δ_i` 中被 `c` subsume 的旧 clauses
3. 删除 `W_i` 中被 `c` subsume 的 witnesses，并把对应 clauses 加入 `R_i`
4. 将被 `c` subsume 的 obligations 从 `O_i` 重新调度到 `O_{i+1}`

如果这个 clause 是在 blocking 期间学到的，它实际上会加强 `L_0 ... L_i`，因此 subsumption 还会继续向更低层传播，直到遇到一个已经 subsume 它的更强 clause 为止。

这部分说明论文不只是加了一个小优化，而是把：

- layer subsumption
- obligation pruning
- triggered pushing

统一进了一个更紧凑的数据结构设计里。

## 论文还提出了一个附加启发式

在 IC3 中，一个 obligation 被成功阻塞后，通常会从 UNSAT core 或 assumptions 中抽出一个 clause，并继续做 clause minimization，尽量删掉不必要的字面量。

论文提出了一个新的 minimization 顺序，叫做 **Witness Directed Minimization (WDM)**。

它的目标是：

- 优先删掉那些会妨碍当前 learned clause subsume 更多 witnesses 的字面量

直觉上，如果新学到的 clause 能 subsume 更多 witness，就能触发更多 pushing，于是更快强化 layers。

因此 WDM 是对 Triggered Pushing 的补充：它不只是“等 witness 被杀掉”，还主动让新 clause 更可能去“杀 witness”。

## 效果如何

论文在 HWMCC 2012 benchmark 上做了实验，结果如下：

- 原始 IC3: 45 个问题
- IC3 + Triggered Pushing: 54 个问题
- IC3 + Triggered Pushing + WDM: 56 个问题

提升主要体现在 UNSAT，也就是“证明安全”的问题上。这和方法本身的目标一致，因为 pushing 的主要价值就是帮助更快形成归纳不变式。

论文还做了一个对照实验：如果几乎去掉 clause propagation，性能会显著下降。这说明 propagation 并不是一个可有可无的后处理步骤，而是 IC3 成功的重要原因之一。

## 这篇论文真正贡献了什么

从算法思想上看，这篇论文的贡献可以概括成三点：

1. 给“失败的 clause pushing”赋予了可复用的信息价值  
   失败时的 SAT 模型不再被丢弃，而是保存成 witness。

2. 给“何时重试 pushing”提供了一个精确触发条件  
   只有当新 clause subsume witness 时，才重新做 SAT 查询。

3. 让 clause propagation 从“批处理阶段”变成“持续在线过程”  
   子句始终尽可能往前推，layer 更强，收敛检测更早。

## 从实现角度可以怎样理解

如果要把这篇论文映射到实际 IC3 代码里，可以抓住下面三个实现点：

- 对每个 pushing 失败的 clause，保存 witness
- 每次新 clause 加入 frame 时，做 clause / witness 的 subsumption 检查
- witness 一旦失效，就把对应 clause 放入待 push 队列

也就是说，它本质上是在原本的 IC3 框架上增加了一层“事件驱动的 pushing 调度”。

## 总结

这篇论文不是在重写 IC3，而是在优化 IC3 中一个非常关键但容易被低估的环节：clause propagation。

它的核心思想非常朴素：

- 既然 SAT 已经告诉了你“为什么某个 clause 推不过去”
- 那就把这个原因保存下来
- 在这个原因没有消失之前，不要浪费时间重试
- 一旦原因消失，立刻重新推动

因此，Triggered Clause Pushing 的本质是：**把 clause pushing 从周期性尝试，改成由 witness 失效事件触发的按需尝试。**
