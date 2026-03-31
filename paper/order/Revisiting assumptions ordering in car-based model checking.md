# Revisiting assumptions ordering in CAR-based model checking 阅读报告

## 1. 论文主题与核心问题

这篇论文关注的问题和前面的 `i-Good Lemmas` 很接近，但切入点不一样：

- 它不是直接研究“什么样的 lemma 更好”；
- 而是研究 **CAR 在调用 SAT solver 时，assumption literals 的顺序为什么会显著影响性能**；
- 特别是，这种顺序会影响 SAT solver 返回的 **unsat core (UC)**；
- 而 UC 又直接决定 CAR 如何收紧 over-approximation frame。

论文的核心观点是：

- 在 CAR 中，`assumption ordering` 不是一个无关紧要的实现细节；
- 它会系统性地影响 bug-finding 的效率；
- 如果对 assumptions 的顺序进行有意识的设计，就可以更快地产生“有用的 UC”，从而提升 CAR 的性能。

作者在已有的 `Intersection + Rotation` 基础上，重新分析其有效原因，并提出了新的方法：

- `CoreLocality`
- `conflict literal` 优先
- `Hybrid-CAR`

整体上，这篇论文的重点更偏向：

- **UC 的形状**
- **SAT 证明速度**
- **unsafe / bug-finding 性能**

而不是像 `i-Good Lemmas` 那样直接围绕 invariant/lemma 收敛来展开。

---

## 2. 论文的基本观察：为什么 ordering 会重要

CAR 和 PDR 都会频繁调用支持 assumptions 的 CDCL SAT solver。

一次 SAT 调用大致可以写成：

```text
SAT(ϕ, A)
```

其中：

- `ϕ` 是 CNF 公式；
- `A` 是 assumptions，对应一个 literal 序列。

现代 SAT solver 会：

- 按给定顺序把 assumptions 作为前几个 decision；
- 在此基础上做 BCP；
- 如果公式不可满足，则从这些 assumptions 中抽取一个 UC。

关键点在于：

- **UC 不仅依赖公式本身，也依赖 assumptions 的顺序**；
- 越早进入搜索过程的 assumption literal，越容易进入最终返回的 UC。

所以在 CAR 中：

- 如果你改变 state cube 中 literal 的排列顺序；
- 就可能改变得到的 UC；
- UC 再反过来影响 frame strengthening；
- 最终影响整个 bug-finding 过程。

论文把这个现象当成主要优化入口。

---

## 3. 论文与已有工作的关系

这篇论文是建立在作者之前关于 CAR assumption ordering 的工作之上的。

之前的工作提出了两个策略：

1. `Intersection`
2. `Rotation`

这篇论文不是完全推翻它们，而是做了三件事：

- 解释 `Intersection` 为什么有效；
- 在此基础上推广成更强的 `CoreLocality`；
- 再配合 conflict literal 与动态切换策略，形成 `Hybrid-CAR`。

所以可以把这篇论文看成：

- 对旧策略的理论化解释；
- 对旧策略的系统升级；
- 对 CAR bug-finding 的进一步工程优化。

---

## 4. 旧方法：Intersection 和 Rotation 是什么

理解这篇论文，先要理解它要改进的对象。

### 4.1 Intersection

`Intersection` 的想法是：

- 记录上一轮得到的 UC；
- 当前如果又在处理一个 state cube `s`；
- 那么把 `s` 中和“上一个 UC”相交的 literals 放到 assumptions 的前面。

直觉上：

- 如果连续几次 UNSAT 都依赖相近的一组变量；
- 那么优先把这些变量排前面，新的 UC 也更容易和旧 UC 相似。

这会让连续得到的 core 更有“局部连续性”。

### 4.2 Rotation

`Rotation` 的想法是：

- 记录最近若干个 failed states 的公共部分；
- 如果搜索反复卡在同一个局部子空间里；
- 就优先把这些公共 literals 放到前面。

它的目标不是让 consecutive cores 更像，而是：

- 利用“最近失败状态的公共结构”来塑造新的 UC；
- 尝试更快地跳出当前被困住的子空间。

### 4.3 旧方法的局限

论文指出：

- `Intersection` 和 `Rotation` 都只对 assumptions 的一部分 literal 给出了顺序；
- 其他大量 literals 仍然是随意放置的；
- 这意味着排序信号还不够充分。

换句话说：

- 旧方法已经说明“排序有用”；
- 但还没有把排序这件事做得足够彻底。

---

## 5. 论文的核心解释：core locality

这是全文最重要的概念。

### 5.1 什么是 core locality

论文提出：

- `Intersection` 之所以有效，
- 是因为它让连续得到的 UC 更相似；
- 作者把这种现象称为 **locality of the cores**。

这里的“locality”可以理解成：

- 当前这次 UNSAT 所需的证明，
- 很可能和上一轮 UNSAT 使用了相近的 clause/变量；
- 如果 assumptions 的排列能让新的 UC 继续靠近旧 UC，
- SAT solver 就更容易更快地找到证明。

### 5.2 它和 SAT 中 locality 的关系

作者把它类比到 CDCL SAT solver 中常见的 locality 现象：

- 决策启发式倾向于优先处理最近参与冲突的变量；
- 这种“局部聚焦”往往能更快地找到相关证明。

在 CAR 里，core locality 的对应物是：

- 不断让新产生的 UC沿着最近的局部结构延续；
- 从而让证明“收敛得更快”。

### 5.3 论文的结论

论文的结论不是简单地说“某种排序经验上更好”，而是进一步强调：

- assumptions ordering 的效果，本质上和 **proof finding speed** 有关；
- 更好的排序，往往意味着更快找到 UNSAT proof；
- 更快找到 proof，又会导致更快的 frame 收紧。

这比“调参式启发式”更进一步，因为它给出了一个较清楚的解释框架。

---

## 6. CoreLocality：对 Intersection 的推广

这是论文里最重要的具体技术。

### 6.1 基本思想

`CoreLocality` 的目标是：

- 不只看“上一个 UC”；
- 而是看 **最近若干个 UC**；
- 用这些 UC 的层次化交集来安排 assumptions 顺序；
- 从而让更多 literals 被有意义地排到前面。

为此，论文引入一个参数：

```text
iLimit
```

它表示：

- 最多回看多少个之前的 UC。

### 6.2 它如何构造顺序

可以把 `CoreLocality` 理解成一种逐层分组：

1. 先取当前 state `s` 与最近 1 个 UC 的交集；
2. 再看与最近 2 个 UC 的交集；
3. 再看与最近 3 个 UC 的交集；
4. 一直到 `iLimit`；
5. 剩余没被覆盖的 literals 再放后面。

因此，它比单纯 `Intersection` 更强，因为：

- `Intersection` 只盯住最近一次 UC；
- `CoreLocality` 让“最近多轮 UC 的公共结构”逐步前移；
- 排序信息更丰富，覆盖的 literals 更多。

### 6.3 和 Rotation 的关系

论文认为：

- `CoreLocality` 某种程度上统一了 `Intersection` 和 `Rotation` 的思想；
- 小的 `iLimit` 更接近 Intersection；
- 继续增大 `iLimit`，会逐渐把多轮失败信息纳入排序过程；
- 这样既保留 locality，又吸收了一部分 Rotation 的效果。

但论文也强调：

- `iLimit` 不是越大越好；
- 太大会把“很旧的 UC 信息”也混进来；
- 反而削弱当前局部证明的聚焦能力。

所以 `CoreLocality` 的关键不是“看得越多越好”，而是：

- 在“更多历史信息”和“当前局部性”之间找平衡。

### 6.4 为什么它比旧方法更强

论文明确指出，`CoreLocality` 比旧方法更强主要因为两点：

1. 它不只排序一小部分 literal，而是决定了更多 literal 的位置；
2. 它把 locality 从“上一轮 UC”推广到了“最近多轮 UC 的结构”。

这让 assumptions 序列更像一个真正有组织的优先级序列，而不是只在最前面插一小段启发式结果。

---

## 7. conflict literal 优先

这是第二个具体优化点。

### 7.1 什么是 conflict literal

论文定义：

- 在 SAT solver 返回的一个 UC 中，
- 最后一个被识别为导致冲突的 literal，
- 称为 **conflict literal**。

作者观察到：

- 这个 literal 和其他 UC 内 literal 不完全等价；
- 它往往更“关键”；
- 去掉它，剩下那些 literal 可能就不再构成 core。

所以作者认为：

- 这种 literal 比普通 UC 成员更值得优先保留和优先排序。

### 7.2 如何使用

论文做法很直接：

- 在后续的 assumption ordering 中，
- 将过去出现过的 conflict literals 再往前放；
- 给予比普通 UC literals 更高的优先级。

可以把它理解成：

- `CoreLocality` 负责利用“哪些 literal 常在最近 cores 里出现”；
- `conflict literal` 优先则进一步区分“谁在 core 里更关键”。

### 7.3 作用

它的目标仍然是：

- 让 SAT solver 更快重现一条短证明路径；
- 更快生成高质量 UC；
- 从而加速 CAR 收紧 O-frames。

这个优化没有改变算法框架，本质上是对排序信号的进一步精炼。

---

## 8. Hybrid-CAR：动态切换配置

这是论文里最工程化、也最有性能冲击力的部分。

### 8.1 问题背景

作者发现一个现实问题：

- `CoreLocality` 的最佳 `iLimit` 并不固定；
- 不同 benchmark 上最优配置不同；
- 甚至同一个 benchmark 的不同阶段，最优配置也可能不同。

另外，CAR 的 `U` 序列可能扩张得很快：

- 前期搜索很快；
- 到后面某一轮，`U` 很大，探索代价急剧上升；
- 算法可能长时间卡住。

### 8.2 基本做法

于是作者提出 `Hybrid-CAR`：

- 运行一段时间后，如果当前配置效果不好；
- 就触发一次 restart；
- 切换到另一组 ordering 配置；
- 同时重置部分搜索状态。

它不是随机重启，而是：

- 有计划地在多个 `CoreLocality` 配置之间切换。

### 8.3 重启时做什么

论文中的做法大致是：

- 给当前配置一个时间配额；
- 如果超过时间还没明显推进，就 restart；
- 保留最低层 `U` 中的状态；
- 清掉其他较高层 `U`；
- 更换新的 ordering configuration；
- 从头继续搜索。

### 8.4 为什么可能有效

它背后的逻辑是：

- 某个 ordering 可能非常适合当前局部搜索，也可能完全不适合；
- 如果始终固定一种排序，算法可能一直困在一个糟糕的搜索轨迹上；
- 周期性切换配置，相当于切换搜索偏好；
- 有机会从另一个角度更快挖出 counterexample。

从思想上说，它有点像：

- 对 assumption ordering 做 portfolio / multi-strategy 调度。

### 8.5 论文里的定位

作者强调：

- `Hybrid-CAR` 不只是比旧的 CAR ordering 更强；
- 还在实验中超过了其他 bug-finding 方法，包括 `ABC-BMC`；
- 因而它是这篇论文最主要的结果之一。

---

## 9. 这篇论文真正解决的是什么问题

从更高层看，这篇论文解决的是：

- **如何让 CAR 在 UNSAT 局部证明上花更少时间**

它并不直接改：

- transition relation；
- frame 语义；
- generalization 的合法性；
- 安全性判定标准。

它改的是一个很细但很关键的层面：

- 同一个 state cube 里的 literals，应该以什么顺序喂给 SAT solver。

而论文认为：

- 这个顺序决定了 proof 搜索会往哪里偏；
- proof 的局部形状决定了 UC 的形状；
- UC 的形状又决定了 CAR 的收敛效率。

所以整篇论文是在打通这条链条：

```text
assumption ordering
→ UC shape
→ proof speed
→ frame refinement speed
→ bug-finding performance
```

---

## 10. 实验结论

论文的实验重点基本都放在 `unsafe` benchmark 上，也就是 bug-finding。

主要结论可以概括为：

1. `CoreLocality` 整体优于之前的 `Intersection + Rotation`
   - 说明“把 assumptions 排得更完整、更系统”是有效的。

2. `conflict literal` 优先是有效增强
   - 它进一步强化了排序信号，帮助更快找到 proof。

3. `Hybrid-CAR` 效果最好
   - 动态切换不同 `CoreLocality` 配置，显著优于静态策略。

4. `Hybrid-CAR` 在 bug-finding 上优于多种已有方法
   - 包括原始 `SimpleCAR`
   - 以及文中比较的 `ABC-BMC` 和若干 BMC/CAR 组合方案。

所以从实验角度，这篇论文最强的结论不是“某一个固定排序最好”，而是：

- **ordering 确实非常重要；**
- **不同 ordering 适合不同阶段；**
- **动态切换 ordering 可以进一步挖出性能。**

---

## 11. 和 i-Good Lemmas 那篇论文的本质区别

这篇论文和 `Searching for i-Good Lemmas to Accelerate Safety Model Checking` 很容易看起来相似，因为两者都：

- 在改 IC3/CAR 的启发式；
- 都和 SAT query 的 literal 排序有关；
- 都不改变算法正确性，只改变搜索路径。

但它们的核心目标其实不同。

### 11.1 i-Good Lemmas 更关注 lemma 质量

`i-Good Lemmas` 关注的是：

- 哪些 clause 更接近最终 invariant；
- 如何让 generalization 倾向于产生可继续前推的 lemma；
- 重点在 safe proving / convergence。

### 11.2 Revisiting assumptions ordering 更关注 UC 与 proof locality

这篇论文关注的是：

- assumption ordering 如何影响 UC；
- UC 的局部连续性如何影响 proof speed；
- 重点在 CAR 的 unsafe checking / bug-finding。

### 11.3 一个更像“引导 clause 生成”，一个更像“引导 SAT 证明”

可以这么理解：

- `i-Good Lemmas`：偏 clause-level / invariant-level；
- `Revisiting assumptions ordering...`：偏 SAT-call-level / core-level。

前者在问：

- “什么样的 lemma 值得留下并优先生成？”

后者在问：

- “什么样的 assumption 顺序更容易诱导出对当前 CAR 搜索最有利的 UC？”

这是两条相邻但不相同的路线。

---

## 12. 对实现者来说，这篇论文最值得关注的点

如果从代码实现角度看，这篇论文最值得先实现的部分依次是：

### 12.1 CoreLocality

原因：

- 改动局部；
- 不改变 CAR 框架；
- 很容易和当前已有 assumption ordering 代码对接；
- 是全文最核心、最可复用的技术。

### 12.2 conflict literal 优先

原因：

- 本质是对已有 UC 信息再加工；
- 成本低；
- 可以看成对 CoreLocality 的轻量增强。

### 12.3 Hybrid-CAR

原因：

- 性能潜力最大；
- 但涉及 runtime 配置切换、restart 和状态管理；
- 工程改动明显更大。

如果只是做一个 baseline：

- 先做 `CoreLocality`
- 再做 `conflict literal`
- 最后才考虑 `Hybrid-CAR`

会更合理。

---

## 13. 我对这篇论文的理解总结

我认为这篇论文最有价值的地方有两个。

第一，它把一个常被视为“实现细节”的东西，即 assumption literal ordering，提升成了一个值得单独研究的性能核心因素。

第二，它不仅给出新启发式，还给出一个较清楚的解释框架：

- 好的 ordering 之所以有效，
- 不是神秘地“更适合这个 benchmark”；
- 而是因为它塑造了更有 locality 的 unsat cores，
- 进而让 SAT proof 更快收敛。

如果说 `i-Good Lemmas` 那篇论文是在回答：

- “怎样让生成出的 lemma 更好？”

那么这篇论文回答的是：

- “怎样让 SAT solver 更快生成对 CAR 更有帮助的 core？”

它们关注点不同，但在工程上其实是可以叠加的。

---

## 14. 一句话总结

这篇论文的核心不是发明一个全新的 CAR，而是证明并利用这样一个事实：

- **在 CAR 中，assumption literals 的顺序会实质性改变 UC 和 proof 的形状；**
- **只要排序得当，CAR 的 bug-finding 能力就能显著提升。**
