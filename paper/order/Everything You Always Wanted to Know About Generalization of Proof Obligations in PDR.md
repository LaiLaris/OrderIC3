# Everything You Always Wanted to Know About Generalization of Proof Obligations in PDR

## 论文信息

- 标题: *Everything You Always Wanted to Know About Generalization of Proof Obligations in PDR*
- 作者: Tobias Seufert, Felix Winterer, Christoph Scholl, Karsten Scheibler, Tobias Paxian, Bernd Becker
- 时间: 2022

## 一句话总结

这篇论文系统研究了 PDR/IC3 中 **proof obligation generalization** 的问题。它首先说明这个问题本质上是一个 `2-QBF` 级别的高复杂度问题，然后全面梳理并比较了已有方法和新方法，覆盖从电路型 transition function 到一般 transition relation、从标准 PDR 到 Reverse PDR、从无约束电路到带 invariant constraints 的场景，最终给出一套“什么方法在什么条件下才正确、才有效”的完整地图。

## 为什么要做

在 PDR 中，当 SAT 求解器发现某个 proof obligation `d` 在 `R_{k-1}` 中有前驱时，通常先得到的是一个**具体状态** `m`。如果直接把这个具体状态作为新的 proof obligation 递归处理，会带来几个问题：

- 处理对象太具体，只是在按单个状态搜索
- 会产生更多 proof obligations
- 学到的 blocking clauses 往往更弱
- frame 强化更慢，整体收敛更慢

因此，PDR 通常会在拿到具体状态 `m` 后，先把它泛化成一个更短的 cube `c`，再把 `c` 当作新的 proof obligation。这就是 **proof obligation generalization**。

论文认为，这一步对 PDR 的性能影响极大，但过去存在几个明显缺口：

- 很多方法只在“transition relation 来自电路函数”时适用
- 一旦 transition relation 更一般，就不知道哪些方法还能用
- 对 invariant constraints、Reverse PDR 等场景缺少系统分析
- 不同 generalization 方法之间的理论强弱、适用前提、实验效果没有被统一比较

所以这篇论文的目标不是只提一个小优化，而是要把 “PO generalization 到底是什么、为什么难、已有方法什么时候成立、还能怎么做” 这件事彻底讲清楚。

## PDR 中它解决的具体问题

在 PDR 中，递归阻塞一个 proof obligation `d` 时，会检查：

`SAT?[¬d ∧ R_{k-1} ∧ T ∧ d']`

如果 SAT，说明找到了一个前驱状态 `m`。  
这时需要做的就是：

- 从完整 minterm `m` 中删掉一些字面量
- 得到更短的 cube `c`
- 但仍保证 `c` 中的每个状态都还是 `d` 的前驱

也就是说，generalization 的目标不是随便变短，而是：

- **删掉尽可能多的 present-state literals**
- **同时保持“仍然是合法 proof obligation”**

## 论文先做了什么：形式化问题并分析复杂度

论文把这个问题形式化成 **PO Generalization Problem (POGP)**：

- 输入是 transition relation `T`
- 一个候选当前状态 cube `c`
- 一个目标后继 cube `d'`
- 问题是：`c` 是否仍然保证所有被它覆盖的当前状态都能经由某个输入转移到 `d'`

这个形式化很重要，因为它把工程中的“删字面量”问题变成了一个严肃的逻辑判定问题。

论文的关键理论结论是：

- **POGP 是 `Π_2^P-complete`**

这表示：

- 精确 generalization 本质上和一个 `2-QBF` 问题一样难
- 想要求出真正最优的 generalized cube，不是简单 SAT 技巧就能轻松解决的
- 过去大量工程方法采用近似手段，不只是 heuristic 偏好，而是问题本身就很难

这一步给整篇论文定了基调：  
**PO generalization 不是一个简单局部优化，而是一个天然高复杂度的问题。**

## 论文做了哪些事情

论文的工作大致分成四部分：

1. 分析 PO generalization 的复杂度
2. 系统研究电路型 transition function 上的近似 generalization 方法
3. 扩展到一般 transition relation，并引入 QBF / MaxQBF 等方法
4. 对各种方法做理论比较和大规模实验比较

## 一、对已有标准方法做系统分析

论文重新梳理了 PDR 中常见的几类 PO generalization 方法：

- `01X-simulation`
- `lifting`
- `justification`

并不是简单复述，而是分析这些方法正确性依赖的 transition relation 性质。

### 1. 01X-simulation

这是 PDR 中很经典的方法。做法是：

- 从一个完整状态 `m` 出发
- 尝试把某些 present-state bit 设为 `X`
- 用三值逻辑在电路上做仿真
- 如果 `X` 没有传播到目标 next-state cube `d'` 的相关输出，就说明这个 bit 不是必须的，可以删除

优点：

- 快
- 实现直接
- 非常适合电路结构

缺点：

- 本质上依赖电路结构和三值仿真
- 是贪心近似，不一定能得到最优 generalization
- 01X 逻辑本身有不精确性

### 2. Justification

这个方法和 01X-simulation 很接近，不过思路更像：

- 不是看“X 会不会传播坏”
- 而是看“哪些 present-state literals 对推出 `d'` 真正必要”

它通过在电路中构造 justification paths，保留那些真正支撑 `d'` 的状态变量赋值。

论文指出：

- justification 得到的结果本质上也是 01X-simulatable 的
- 所以它在 generalization 强度上与 01X 系方法关系很近

### 3. Lifting

lifting 是另一类非常有代表性的方法。

思路是：

- SAT 查询得到完整赋值 `m` 和输入 `i`
- 如果 transition relation `T` 是电路函数，那么 `m ∧ i` 会决定唯一后继
- 因而查询 `SAT?[m ∧ i ∧ T ∧ ¬d']` 必然 UNSAT
- 再利用这个 UNSAT 证明删除 `m` 中不必要的字面量

lifting 的关键优点是：

- 它比纯仿真更精确
- 能得到比简单 01X-greedy 更强的 generalization

但论文对 lifting 做了很细的限制分析，指出它并不是普适方法。

## 二、为什么已有方法不总是能用

这是论文很有价值的一部分：它不只是介绍方法，而是明确指出**什么时候方法会失效**。

### 1. Lifting 对 transition relation 有严格前提

论文指出 lifting 要正确，transition relation 必须足够像“函数”。

#### 如果不是 right-unique

如果同一个当前状态和输入可以有多个后继，那么：

- `m ∧ i` 未必唯一决定一个后继
- 即使有一个后继落入 `d'`，也可能还有另一个后继不落入 `d'`
- 这会破坏 lifting 依赖的 UNSAT 结构

结果是：

- lifting 可能找不到该有的 generalization
- 或者其逻辑基础不再成立

#### 如果不是 left-total

如果某些当前状态 / 输入组合根本没有后继，那么：

- lifting 查询可能因为“没有后继”而 UNSAT
- 但这种 UNSAT 不是因为“所有状态都能到 `d'`”
- 而是因为“某些状态根本走不出去”

这就会导致：

- 删除某些 literals 后看起来仍 UNSAT
- 但 generalized cube 其实不是真正的 proof obligation
- 甚至可能引入错误结果

论文强调：

- 缺失 right-uniqueness 往往导致 lifting 失败或变弱
- 缺失 left-totality 更危险，因为可能导致**错误 generalization**

这点非常重要，因为很多真实系统并不是理想电路函数。

### 2. invariant constraints 会破坏这些前提

即使系统本质上来自电路，只要再加上 invariant constraints，transition relation 也可能不再 left-total。

这时：

- 一些标准 lifting 实现可能会产生错误结果
- 论文甚至给出了实验中标准实现出错的例子

因此，论文专门讨论了带 invariant constraints 的情况，说明哪些方法还能直接用，哪些需要修改，哪些应该避免。

## 三、论文提出和引入了哪些新方法

除了分析已有方法，论文还系统引入了多种以前在 PDR 里没有被充分使用的方法。

### 1. IGBG

全称 **Implication Graph Based Generalization**。

基本思想是：

- 在电路 transition function 上，给定完整赋值 `m ∧ i`
- SAT 求解 `m ∧ i ∧ T` 时主要靠布尔传播（BCP）
- 那么可以直接回溯 implication graph
- 找出哪些 present-state literals 真正参与推出了目标 next-state cube `d'`
- 只保留这些 literals

它和 lifting 相比，特点是：

- 更像在利用 SAT 的传播结构
- 不需要完整做 lifting 风格的 UNSAT core 提取
- 非常适合电路函数

论文指出 IGBG 的结果本质上也是 01X-simulatable 的，但它的效果和效率都很有竞争力。

### 2. MS01X

论文把 01X generalization 改造成了一个 **MaxSAT 优化问题**。

目标是：

- 尽量多把 present-state bits 设成 `X`
- 同时仍保证在固定输入下，所有转移都会落入 `d'`

相比 greedy 01X-simulation：

- 它不是一个 bit 一个 bit 地局部尝试
- 而是整体求一个“删得尽可能多”的解

因此：

- 一般能得到更强的 generalization
- 但计算代价也更高

### 3. S01X

这是 MS01X 的一个 SAT 近似版本。

核心思想是：

- 仍然使用 01X 编码
- 但不用 MaxSAT 求全局最优
- 而是用 SAT 求一个局部更好的结果

可以理解为：

- 比纯 greedy 01X 更“优化导向”
- 但又比 MaxSAT 便宜

### 4. GeNTR

这是面向**一般 transition relation** 的 generalization 方法。

思想和 lifting 有点像，但不再依赖 transition relation 是函数，而是从满足 `T` 的完整赋值出发，转而利用 `¬T` 的不可满足性来做泛化。

它的重要意义在于：

- 不依赖电路函数结构
- 在更一般的关系模型上也能工作

### 5. Cover 类方法

当 transition relation 很一般时，论文还引入了多种 **cover-based** 方法。

它们的核心思想是：

- 从完整满足赋值出发
- 找一个更小的 partial assignment
- 仍然能“覆盖”使 transition relation 成立的关键 clauses

具体包括：

- greedy cover
- ILP cover
- SAT-based cover

这些方法本质上是在做“最小满足赋值 / hitting set / covering”一类问题。

特点是：

- 不依赖函数性
- 更通用
- 但通常 generalization 能力弱于结构化电路方法

### 6. QBF 和 MaxQBF

这是论文最“精确”的方法族。

既然 POGP 本质是 `2-QBF` 问题，那么自然可以：

- 用 `greedy QBF` 逐字面量测试是否可删
- 用 `MaxQBF` 直接求“删最多字面量”的最优解

这类方法的特点非常明确：

- **适用于一般 transition relation**
- **理论上最强**
- **计算代价最高**

论文特别强调：

- `QBF` 是精确判定
- `MaxQBF` 可以给出最优 generalization

这使得它们不仅本身可用，还能作为“评估其他近似方法离最优有多远”的参照。

## 四、论文如何处理特殊场景

### 1. circuits with invariant constraints

论文指出，这种场景下 transition relation 可能不再 left-total，因此直接用标准 lifting 是危险的。

它给出几种解决方式：

1. 改用适用于一般 transition relation 的方法
2. 如果 right-unique 仍成立，可以使用 IGBG
3. 对 01X-simulation 增加对 invariant constraint 的检查
4. 把系统转换成 left-total / right-unique 的形式
   - 比如为非法转移加 self-loop
   - 或者引入 dead-end state
5. 修改 lifting 查询，把 invariant constraint 单独纳入

这说明论文并不是只做理论分析，而是在工程上给出了明确处理方案。

### 2. Reverse PDR

论文还分析了 Reverse PDR 的情况。

它指出：

- 把 transition relation 反过来之后，会得到不同的结构性质
- 某些近似方法在这种 left-unique 场景下会被严重限制
- 真正能 generalize 的，往往是
  - QBF / MaxQBF 方法
  - 或专门的 structural 方法

所以：

- forward PDR 里有效的方法，不能直接假设在 Reverse PDR 中也一样有效

## 五、论文怎样比较不同方法

论文不是简单列方法，而是从两个维度进行比较：

### 1. 正确性和适用条件

作者特别关注：

- 这个方法需要 `T` 是函数吗
- 需要 left-total 吗
- 需要 right-unique 吗
- 遇到 invariant constraints 时还正确吗
- 能否用于一般 transition relation
- 能否用于 Reverse PDR

这使得论文像一份“PO generalization 方法适用手册”。

### 2. generalization 强度

作者还分析不同方法从理论上谁更强、谁更弱。

大致可以这样理解：

- `MaxQBF` 是最强的，因为它求最优解
- `QBF` 是精确的，但是否达到最优取决于策略
- `MS01X` 比普通 01X-simulation 更强
- `IGBG`、justification、01X-simulation、S01X` 处于同一大类近似方法谱系中
- cover 类方法通常更弱，但更通用

论文也强调：

- “理论上更强”不等于“整体运行时间一定更好”
- 因为 stronger generalization 往往也更贵

## 六、实验结果说明了什么

论文在多个基准集上做了实验，包括：

- HWMCC benchmark
- 带 invariant constraints 的 benchmark
- AI Planning benchmark

整体结论非常明确。

### 1. 标准方法并不总是最好

论文发现，传统“标准方法”如：

- lifting
- greedy 01X-simulation

并不总是最优选择。

在普通硬件 PDR 上：

- `IGBG`
- `MS01X / IGBG` 组合

通常优于这些标准 baseline。

### 2. 01X-based / lifting-based 方法在结构允许时通常优于 cover 方法

当 transition relation 是电路函数并满足所需结构时：

- 结构化方法一般能得到更强 generalization
- 且整体性能通常优于 cover 类方法

### 3. 一般 transition relation 上，QBF / MaxQBF 更有意义

在 AI Planning 这类场景中：

- transition relation 不是简单函数
- 结构化电路方法未必适用

这时：

- cover 类方法提供较便宜的通用 generalization
- QBF / MaxQBF 虽然昂贵，但在部分 benchmark 上明显带来好处

特别是某些 planning benchmark 中，MaxQBF 虽然贵，但能显著减少：

- 打开的 time frames 数量
- 学到的 clauses 数量
- 总体运行时间

### 4. 最强的 generalization 不一定带来最好的总性能

论文反复强调一个现实结论：

- 最强 generalization 方法，未必是总时间最优的方法

原因是：

- generalization 本身也要花时间
- 如果一个方法虽然 generalization 很强，但每次代价过高，整体上可能得不偿失

因此最好的策略往往是平衡：

- generalization 质量
- 单次代价
- 对后续 PDR 搜索的收益

## 七、这篇论文的核心贡献

这篇论文最重要的贡献可以概括成下面几条。

### 1. 把 PO generalization 这个问题正式定义清楚

它不再只是工程经验，而被严格形式化为一个逻辑问题，并证明其复杂度为 `Π_2^P-complete`。

### 2. 说明很多已有方法并非普适

特别是 lifting，在不满足相应结构条件时，不只是“效果不好”，而是可能**不正确**。

### 3. 为不同类型的 transition relation 给出一整套方法谱系

从电路函数到一般 transition relation，从近似方法到精确方法，从普通 PDR 到 Reverse PDR，论文都给出了系统方法。

### 4. 引入并验证了多种在 PDR 中不常用的新方法

尤其包括：

- IGBG
- MS01X
- S01X
- GeNTR
- QBF / MaxQBF

### 5. 说明“没有一种方法在所有场景下都最好”

真正合理的结论不是“统一替换成某一个最强方法”，而是：

- 根据 transition relation 的结构性质选择方法
- 甚至组合多种方法
- 用便宜方法先做，再用更强方法补充

## 八、从实现角度该怎样理解这篇论文

如果把这篇论文映射回 PDR 的实现流程，它研究的就是：

1. SAT 查询得到具体前驱状态 `m`
2. 调用 `SatGeneralization(m)`
3. 生成更短的 cube `c`
4. 用 `c` 作为新的 proof obligation

论文整篇都在研究：

- `SatGeneralization(m)` 到底该怎么做
- 依赖哪些 transition relation 条件
- 哪些做法只是近似
- 哪些做法是精确的
- 哪些做法整体上更值这个代价

所以它本质上是在给 PDR 的 `SatGeneralization` 阶段做一份“理论 + 方法 + 工程”的完整说明书。

## 总结

这篇论文最大的价值在于，它把 proof obligation generalization 从几个零散技巧，提升成了一个被系统研究的问题。

它说明了：

- 为什么 PO generalization 对 PDR 至关重要
- 为什么这个问题本身天然很难
- 为什么已有很多方法只能在特定结构下成立
- 一般 transition relation 上还能怎么做
- 不同方法在理论和实践上各自强在哪里、弱在哪里

如果用一句话概括，这篇论文做的事情就是：

- **把 PDR 中 proof obligation 的 generalization 问题，从“经验 heuristic”上升为“可形式化、可分类、可比较、可工程落地”的系统研究对象。**
