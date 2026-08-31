## VirtECom 1.0

**An Agent-Based Model of Economic Complexity in Digital Marketplaces**

VirtECom is a Netlogo model that explores whether mechanisms commonly associated with Economic Complexity Theory can emerge in digital marketplaces. While Economic Complexity studies traditionally analyze Country-Product networks, VirtECom investigates whether analogous patterns arise in Seller-Product networks driven by buyer demand, capability accumulation, and adaptive learning. The model is based on the following theoretical sources:

- Economic Complexity Theory (Hidalgo & Hausmann, 2009)
- Evolutionary and Neo-Schumpeterian Economics (Bruun, 2003; Pyka & Fagiolo, 2007)
- Digital Marketplace Agent-Based Models (Li et al., 2025)
- Innovation Systems and Knowledge-Based Dynamics (Antonelli & Ferraris, 2011)

Inspired by Hidalgo and Hausmann's framework, VirtECom reinterprets the traditional Country-Product network as a Seller-Product network. Sellers possess capabilities that determine the products they can offer, while buyers generate demand through preferences and purchasing decisions.

The model seeks to understand how market structure, product diversity, and buyer demand affect the emergence of complexity-related indicators such as diversification, ubiquity, and first-order reflections.

**ASSUMPTIONS**
This model is based on the following simplifying assumptions:

- All capabilities are equally likely to appear.
- All capabilities are equally valuable.
- Sellers initially possess between 4 and 7 capabilities.
- Products require between 2 and 5 capabilities.
- Capabilities can be acquired but cannot be lost.
- Sellers never leave the marketplace.
- Products never disappear.
- Seller-product links can be created but cannot be removed.
- Learning occurs internally and not through knowledge spillovers.

**Known Limitations**

VirtECom should be interpreted as a proof-of-concept model. Current limitations include:

- Capabilities are homogeneous and equally likely.
- Capability complementarities are not represented.
- Seller-product networks tend to be denser than empirical Country-Product networks.
- Capabilities can be acquired but not lost.
- Sellers cannot fail or exit the marketplace.
- Knowledge spillovers between sellers are not represented.
- Consumer attention constraints are not modeled.
- The model does not calculate ECI (Economic Complexity Index) or PCI (Product Complexity Index); it currently implements diversification (kc₀), ubiquity (kp₀), and first-order reflections (kc₁ and kp₁).

## RESEARCH QUESTIONS

VirtECom was designed to explore the following questions:

1. Can mechanisms commonly associated with Economic Complexity Theory emerge in digital marketplaces?

2. How does the structure of seller capabilities influence product diversification and ubiquity?

3. Does buyer demand indirectly affect structural complexity through capability accumulation?

4. How do changes in the numbers of sellers, buyers, and products alter the properties of the seller-product network?

5. Can marketplaces exhibit complexity-related dynamics similar to those observed in national economies?

## HOW IT WORKS

VirtECom contains three classes of agents:

**Sellers**

Sellers possess capabilities and can offer products if they have all the capabilities required by a product. Seller complexity is operationalized as the number of capabilities currently possessed by a seller.

Each seller maintains:

- Capability set
- Revenue
- Profit
- Complexity
- Diversification
- KC1

**Products**

Products require specific capabilities. Each product maintains:

- Required capabilities
- Product category
- Price
- Quality
- Complexity
- Sales
- Ubiquity
- KP1

**Buyers**

Buyers possess:

- Budget
- Preferences

Buyers search for products that match their preferences and budget constraints.

**Bipartite Network Formation**

A seller-product link is formed whenever:

RequiredCapabilities ⊆ SellerCapabilities

A seller-product link does not represent a transaction. It represents the capability of a seller to offer a product given its current capability portfolio. This bipartite structure serves as the analog of the Country-Product matrix commonly used in Economic Complexity research introduced by Hidalgo and Hausmann (2009).

KC1 = Mean ubiquity of products connected to a seller.

KP1 = Mean diversification of sellers connected to a product.

These indicators represent first-order reflections inspired by the Method of Reflections proposed in Economic Complexity Theory (Hidalgo & Hausmann, 2009).

## HOW TO USE IT

Press "setup" to initialize the simulation setup. By pressing "go", the model starts the simulation. During each tick:

- Buyers search for products.
- Purchases occur.
- Sellers collect revenue and profit.
- Sellers may acquire new capabilities.
- Seller-product connections may expand as sellers acquire new capabilities.
- Network-based diversification, ubiquity, and first-order reflection metrics are updated.

The main parameters of this systems are "Number-of-Sellers," "Number-of-Customers," and "Number-of-Products"

## THINGS TO NOTICE

Pay attention to the following indicators.

**Diversification**: Average number of products sellers are connected to. This is the equivalent to kc0 in economic complexity. 

**Ubiquity**: Average number of sellers connected to a product. This is the equivalent to kp0 in economic complexity.

KC1 refers to Average ubiquity of products associated with a seller, while KP1 is the Average diversification of sellers associated with a product.

**Network density** is calculated as count offers / (count sellers * count products) which measures how dense the seller-product matrix is.

**Mean Complexity** is the average seller complexity.

## THINGS TO TRY

Increase the number of sellers while keeping buyers and products constant.

Observe how diversification changes.

Increase the number of products while holding other parameters fixed.

Observe changes in ubiquity and network density.

Increase the number of buyers while holding sellers and products constant.

Observe whether demand influences complexity through learning and capability accumulation.

Use BehaviorSpace to explore combinations of:

Sellers: 20, 50, 100
Customers: 20, 100, 500
Products: 10, 25, 50

## EXTENDING THE MODEL

Potential future extensions include:

- Rare capabilities
- Capability complementarities
- Capability depreciation
- Knowledge spillovers between sellers
- Entry and exit of firms
- Creative destruction mechanisms
- Consumer attention constraints
- Reputation systems
- Advanced Economic Complexity measures (ECI-like indicators)

## NETLOGO FEATURES

VirtECom 1.0 makes use of:

- Multiple agent breeds
- Bipartite seller-product networks
- Capability-based product eligibility
- Buyer preference matching
- Adaptive capability acquisition
- Dynamic network expansion
- BehaviorSpace-compatible experimentation


## INITIAL EXPERIMENTAL FINDINGS
BehaviorSpace experiments were conducted using:

Sellers = {20, 50, 100}
Customers = {20, 100, 500}
Products = {10, 25, 50}

Preliminary results indicate that:

- Increasing the number of sellers generally increases diversification.
- Buyer demand may indirectly influence complexity through capability accumulation.
- Product variety influences network structure and ubiquity.
- The resulting seller-product network is denser than empirical country-product matrices, motivating future work on rare capabilities and capability complementarities.

These findings suggest that demand-side dynamics may influence structural complexity indirectly through capability acquisition and network expansion.

## RELATED MODELS

VirtECom combines ideas that are typically studied separately in the literature.

**Economic Complexity Models**

The conceptual foundation of VirtECom is inspired by the Economic Complexity framework of Hidalgo and Hausmann (2009), where economic development emerges from the accumulation and combination of productive capabilities. VirtECom reinterprets the traditional Country-Product matrix as a Seller-Product network, allowing Economic Complexity concepts to be explored in digital marketplace environments.

**Evolutionary and Neo-Schumpeterian Models**

VirtECom is related to evolutionary models of innovation and economic change that emphasize learning, adaptation, capability accumulation, and path dependence. In particular, the model shares conceptual similarities with:

- Schumpeterian models of innovation and creative destruction.
- Neo-Schumpeterian Agent-Based Models (Pyka & Fagiolo, 2007).
- Innovation Systems models focused on knowledge accumulation and capability development.

**Digital Marketplace Models**

The model is also related to Agent-Based Models of online marketplaces that investigate:

- Buyer preferences
- Product choice
- Market concentration
- Platform dynamics

Unlike most marketplace models, VirtECom explicitly incorporates capability-based production constraints and Economic Complexity indicators.

**Future Developments**

Future models will move closer to evolutionary and Schumpeterian theories of economic development through:

- Rare capabilities
- Capability complementarities
- Capability depreciation
- Knowledge spillovers
- Firm entry and exit
- Creative destruction mechanisms

Future versions will investigate how combinations of scarce and complementary capabilities influence the emergence of complexity, specialization, market concentration, and innovation in digital marketplaces.

## CREDITS AND REFERENCES

Antonelli, C., & Ferraris, G. (2011). Innovation as an emerging system property: an agent based simulation model. *Journal of Artificial Societies and Social Simulation, 14*(2), 1. https://jasss.soc.surrey.ac.uk/14/2/1.html

Bruun, C. (2003). The Economy as an Agent-Based Whole-Simulating Schumpetarian Dynamics, *Industry and Innovation, 10*(4), 475-491 https://doi.org/10.1080/1366271032000163694

Hidalgo, C. A., & Hausmann, R. (2009). The building blocks of economic complexity. *Proceedings of the national academy of sciences, 106*(26), 10570-10575.

Li, T., Wang, S., Zhou, D. & Razzaq, A. (2025). Consumer attention and market concentration in e‑commerce: an agent‑based perspective, *Journal of Economic Interaction and Coordination, 20* 959–985 https://doi.org/10.1007/s11403-025-00443-5

Pyka, A., & Fagiolo, G. (2007). Agent-based modelling: A methodology for neo-Schumpeterian economics'. In Hanusch, H. and Pyka, A. (Eds). *Elgar companion to neo-Schumpeterian economics,* (pp. 467 – 492), Edward Elgar Publishing, Cheltenham, UK
