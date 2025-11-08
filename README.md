## New York City School Bus Delay Analytics:Leveraging Data to Improve Network Reliability 
## 纽约市校车延误分析：利用数据分析提升运输网络可靠性

## Project Introduction / 项目简介

This project investigates the persistent issue of school bus delays in New York City using a data-driven analytics approach. Leveraging over 740,000 official records from NYC Open Data (2015–2025), the team conducted systematic data cleaning, descriptive statistics, and advanced modelling (CART, Random Forest, Bootstrap, and regression analysis). The results identified key delay drivers—bus operators and routes—and generated actionable recommendations for improving operational reliability and ensuring fair access to education for all students.
本项目通过数据分析方法研究纽约市长期存在的校车延误问题。基于纽约市开放数据平台2015–2025年的74万条官方记录，项目团队进行了系统的数据清洗、描述性统计及高级建模（包括CART决策树、随机森林、Bootstrap与回归分析）。结果发现校车延误的主要驱动因素为运营商与线路特征，并据此提出针对性的改进建议，以提升运营可靠性并保障所有学生的公平受教育机会。

## Business Situation and Problem Setting / 业务背景与问题定义

New York City’s school bus system serves over 150,000 children, including 66,000 with disabilities or in temporary housing. However, in 2023/24 alone, over 80,000 delays were recorded with a 20% rise in complaints. This reliability crisis affects underprivileged students most severely, causing missed classes and deepening educational inequality.
The business problem was thus defined as:“What are the main drivers of school bus delays in NYC, and how can the city improve network reliability?”
纽约市共有超过15万名学生依赖校车通勤，其中6.6万名是残障或临时住房儿童。2023/24学年校车延误超过8万次，投诉量上升20%，严重影响弱势群体的上学机会。因此，核心问题定义为：“纽约市校车延误的主要原因是什么？城市应如何提升运输网络的可靠性？”

## Data Source / 数据来源

NYC Open Data – Bus Breakdown and Delays (2015–2025) https://data.cityofnewyork.us/Transportation/Bus-Breakdown-and-Delays/ez4e-fazm

## Data Cleaning / 数据清洗

The original dataset “Bus Breakdown and Delays” from NYC Open Data (2015–2025) contained 748,482 records and 21 columns, but was plagued by duplicates, missing entries, and inconsistent time formats.
After extensive cleaning, 582,073 valid records and 16 key variables remained.

Key cleaning steps included:
Removing irrelevant or duplicate columns (e.g., Incident_Number, Created_On).
Eliminating records with missing or unrealistic data (e.g., over 1,000 students on a bus).
Standardizing delay times to unified minutes (e.g., “1h30m” → 90 min, “10–15m” → 12.5).
Unifying inconsistent company names through prefix-matching and uppercase mapping.

These cleaning procedures enabled accurate modeling and highlighted severe issues in NYC’s data-entry system, which relied on manual inputs.

Recommendations: Implement input format restrictions (numeric-only for durations, dropdowns for categorical fields) to enhance data-entry systems to prevent format inconsistencies and improve real-time reporting efficiency.

原始数据集来源于纽约市开放数据平台（2015–2025），共包含 748,482 条记录、21 个变量。由于重复、缺失值和格式不一致，数据质量较差。经过系统清洗后，保留 582,073 条有效记录、16 个核心变量。

主要清洗工作包括：
删除无关或重复列（如 Incident_Number、Created_On）。/n
去除含有缺失值或不合理数据的记录（如学生人数>1000）。
将延误时间统一转换为分钟单位（如 “1h30m”→90、“10–15m”→12.5）。
统一公司名称格式，通过前缀映射和大写标准化。

这些步骤保证了模型分析的准确性，并暴露出纽约市数据录入系统的严重手工输入问题。建议政府在系统中加入格式限制与下拉菜单选择，防止录入错误，从而改善数据系统以提高一致性和效率。

## Descriptive Analysis / 描述性分析

1.Delay Duration Statistics / 延误时间分布特征

After cleaning, delay durations were found to range between 8 and 76 minutes, with:
Mean: 38.7 min
Median: 38 min
1st Quartile (Q1): 23 min
3rd Quartile (Q3): 53 min

Since school buses typically arrive 20–25 minutes before classes, 75% of delays cause students to miss class time. This pattern indicates that school bus delays have a direct educational impact, particularly for low-income or disabled students who depend on these services.

中文：
数据清洗后发现延误时长介于 8–76 分钟 之间，统计结果如下：
平均值（Mean）：38.7 分钟
中位数（Median）：38 分钟
第一四分位（Q1）：23 分钟
第三四分位（Q3）：53 分钟

由于校车通常提前 20–25 分钟到校，约 75% 的延误会导致学生迟到或缺课。这一发现表明，校车延误不仅是运营问题，更会加剧教育机会的不平等。

2. Delay Cause Analysis / 延误原因分析

Frequency analysis of delay causes revealed that traffic overwhelmingly dominates, but operator-related issues are also major contributors:

Heavy Traffic ___  Most common cause; beyond direct control but related to routing ___ External
Mechanical Problem, Won’t Start, Problem Run, Flat Tire	 ___  Operator-controlled, reflect maintenance and management issues ___ Operator
Weather Conditions ___ Natural factor ___ External
Other	Mixed / undefined causes ___ Mixed

This finding suggests that operational management and routing strategies—rather than environmental conditions—are the main levers for improvement.

延误原因频率分析显示，交通拥堵是首要原因，但运营商可控的机械与调度问题同样重要：
交通拥堵 ___ 延误最常见原因，虽难控制但可通过线路优化缓解 ___ 外部
机械故障、车辆未启动、运行异常、轮胎爆裂 ___ 属于公司可控问题，反映维修与调度管理不善 ___ 运营商
天气因素 ___ 自然不可控因素 ___ 外部
其他	未定义或混合原因 ___ 混合

因此，改进校车准点率的关键应集中于 优化路线与提升运营管理，而非仅关注外部环境

3. Operator Error Index / 运营商差异初步发现

The Operation Error Index (OEI)—the ratio of operator-controlled delay causes to total causes—varied significantly across companies.
Some operators were responsible for a disproportionately high share of delays, confirming that operator performance is a critical determinant of overall reliability.
This metric served as the foundation for subsequent CART, Random Forest, and Bootstrap analyses to pinpoint problematic companies.

中文：
通过计算运营错误指数（OEI）（运营商可控延误占比），发现不同公司之间差异显著。
部分公司在延误事件中占比极高，说明运营商绩效是决定校车可靠性的重要因素。
这一结果为后续 CART、随机森林及 Bootstrap 模型分析奠定基础

4. Summary Insights / 阶段性结论

Most school bus delays last long enough to cause educational disruption.
Traffic is the main external cause, but operator-controlled issues are equally impactful.
Operator performance differs drastically—some companies perform significantly worse.

大多数校车延误严重到足以影响上课。
交通虽是主要外部因素，但运营商可控问题同样关键。
不同公司表现差距极大，部分运营商延误率显著偏高。

## Advanced Analytics / 高级分析与建模

### CART & Random Forest Models

Both models confirmed **Bus Operator** and **Route Number** as the only significant predictors (≈97% importance). Natural factors like “Heavy Traffic” had low predictive power, implying delays often mask internal operational issues.

CART 与随机森林模型共同验证： **运营商名称**与**线路编号**是唯一显著影响延误时间的变量（解释力达97%）。自然因素如交通、天气仅占极小比例，说明许多延误源于内部运营问题而非外部环境。


### Bootstrap Analysis of Problematic Operators

Six problematic operators were identified and categorized:
* **Mechanical Problems:** Jofaz, SNT, Boro, First Steps
* **Operational Problems:** Leesel
* **Mixed Issues:** Quality Transportation
  Targeted measures: fleet upgrades, management improvement, or contract replacement.

Bootstrap 分析识别出6家问题运营商：
* **机械故障型：**Jofaz、SNT、Boro、First Steps
* **运营调度型：**Leesel
* **混合型：**Quality Transportation
  针对性措施包括车辆更新、运营优化或终止合同。

### Regression-based Operational Performance Index (OPI)**

An OPI (0–100) was built using four weighted dimensions:

* Efficiency (40%)
* Reliability (30%)
* Responsiveness (20%)
* Compliance (10%)
  Operators with OPI <70 lose contracts; ≥70 get warnings but remain with improvement plans.

Result:
* **Non-renewed:** Jofaz, First Steps, Quality
* **Warned but retained:** Boro, SNT, Leesel

通过线性与逻辑回归模型构建**运营绩效指数（OPI）**，衡量四个维度：效率40%、可靠性30%、响应速度20%、合规性10%。
OPI低于70的公司合同不再续签；高于70的公司警告并协助改进。

最终结果：
* **不续签：**Jofaz、First Steps、Quality
* **保留但警告：**Boro、SNT、Leesel。

### **Problematic Routes**
Several route numbers (e.g., M136, Q863, K132, N534) consistently showed long delays. City officials should re-route buses to avoid congested areas or adjust schedules to include buffer time.

部分线路（如 M136、Q863、K132、N534 等）表现出持续高延误。建议城市交通部门重新规划路线以避开拥堵区域，或调整时刻表增加缓冲时间。

## Key Insights & Recommendations / 核心结论与建议
1. **Focus Factors:** Bus operator and route number are the only significant delay drivers.
2. **Operator Strategy:** Replace three low-performing companies; cooperate with three to fix internal issues.
3. **Routing Strategy:** Re-route high-delay routes and revise schedules.
4. **Data Quality:** Introduce system-based validation to prevent inconsistent input.
5. **Equity Impact:** Reducing delays directly supports educational equality.

1. **关键影响因素：**校车运营商与线路编号是唯一显著延误因素。
2. **运营策略：**终止3家低绩效公司合同，与其余3家合作改进。
3. **路线策略：**优化拥堵线路，调整校车时刻表。
4. **数据质量：**在数据系统中增加输入限制，防止格式错误。
5. **公平性影响：**减少延误有助于弱势学生的教育公平。

