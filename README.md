# Towards a General Methodology of Bridging Ideological Spaces

## Authors

Tzu-Ping Liu (UC Davis) <br>
Gento Kato (Nazarbayev University) <br>
Samuel Fuller (UC Davis)

## Working Paper

*In progress*

## Abstract

Bridging ideal points estimated in two different groups of individuals (e.g., politicians and voters) into a common ideological space is an important, but relatively troubled branch of the ideological scaling literature. The most common procedure is **joint-scaling**, which is to merge two groups prior to the joint estimation of ideal points. This procedure has been criticized to ignore the possible differences in structures of ideological space between groups. Alternative approach, **dimensional-mapping**, accommodates this issue by estimating a transformation model of two ideological spaces. Existing dimensional mapping techniques, however, are of limited use, since they are not applicable to multi-dimensional and non-parametrically estimated ideal points and require "real" anchors (i.e., individuals who exist in two groups at the same time). Therefore, we propose a more generally applicable technique of dimensional-mapping that incorporates ideal points that deal with the above issues. Also, our method allows to subsample small numbers of individuals and set them as synthetic anchors: it does not require real anchors. We demonstrate the utility of our methodology by comparing its performance with that of joint scaling and existing dimensional mapping methodology using two sets of voter-politician data from the United States and Japan. The result suggests that, while it makes less stringent assumptions than joint scaling and is more widely applicable than existing dimensional mapping techniques, our method can generate bridged ideal point estimates comparable to those generated from previous methodologies.

## Figures/Tables

### Figure 1: Variations in Forms of Geometric Transformation

[Replication Codes](./Codes/Figure_1.R)

![Figure 1](https://github.com/tzuliu/Towards-a-General-Methodology-of-Bridging-Ideological-Spaces/raw/master/Outputs/illustration/transform_illustration.png)

### Figure 2/Table 1: Comparing Bridging Results for US Senators and Voters

Replication Codes:
 * [0. Rearranging Data](./Codes/Figure_2/Figure_2_0_data_rearrange.R)
 * [1. Estimating Ideal Points](./Codes/Figure_2/Figure_2_1_ip_estimation.R)
 * [2. Exporting Figure/Table](./Codes/Figure_2/Figure_2_2_plot.R)

![Figure 2](https://github.com/tzuliu/Towards-a-General-Methodology-of-Bridging-Ideological-Spaces/raw/master/Outputs/application/senator_figure.png)

### Figure 3/Table 2: Comparing Bridging Results for Japanese Candidates and Voters (UTAS 2012)

Replication Codes:
 * [0-1. Rearranging Data](./Codes/Figure_3/Figure_3_0_data_rearrange1.R)
 * [0-2. Rearranging Data](./Codes/Figure_3/Figure_3_0_data_rearrange2.R)
 * [1. Estimating Ideal Points](./Codes/Figure_3/Figure_3_1_ip_estimation.R)
 * [2. Exporting Figure/Table](./Codes/Figure_3/Figure_3_2_plot.R)

![Figure 3](https://github.com/tzuliu/Towards-a-General-Methodology-of-Bridging-Ideological-Spaces/raw/master/Outputs/application/utas12_figure.png)

### Figure 4: Density Plots of Candidate's Positions with Different Amount of Extra Voter Samples (UTAS 2012)

Replication Codes:
 * [0-1. Creating Data (0 extra samples)](./Codes/Figure_4/Figure_4_0_data_creation0000.R)
 * [0-2. Creating Data (20 extra samples)](./Codes/Figure_4/Figure_4_0_data_creation0020.R)
 * [0-3. Creating Data (50 extra samples)](./Codes/Figure_4/Figure_4_0_data_creation0050.R)
 * [0-4. Creating Data (100 extra samples)](./Codes/Figure_4/Figure_4_0_data_creation0100.R)
 * [0-5. Creating Data (500 extra samples)](./Codes/Figure_4/Figure_4_0_data_creation0500.R)
 * [0-6. Creating Data (1000 extra samples)](./Codes/Figure_4/Figure_4_0_data_creation1000.R)
 * [1. Rearranging Data](./Codes/Figure_4/Figure_4_1_data_rearrange.R)
 * [2. Exporting Figure](./Codes/Figure_4/Figure_4_2_plot.R)

![Figure 4](https://github.com/tzuliu/Towards-a-General-Methodology-of-Bridging-Ideological-Spaces/raw/master/Outputs/simulation/utas12_sim_400_outsample_density.png)

### Figure 5: Correlation between Estimated and Real Positions

Replication Codes:
 * [0. Creating Data](./Codes/Figure_5/Figure_5_0_data_creation.R)
 * [1. Exporting Figure](./Codes/Figure_5/Figure_5_1_plot.R)

![Figure 5](https://github.com/tzuliu/Towards-a-General-Methodology-of-Bridging-Ideological-Spaces/raw/master/Outputs/simulation/mcmc_corr_plot_20.png)

### Figure A1/Table A1: Comparing Bridging Results for Japanese Candidates and Voters (UTAS 2009)

Replication Codes:
 * [0-1. Rearranging Data](./Codes/Figure_A1/Figure_A1_0_data_rearrange1.R)
 * [0-2. Rearranging Data](./Codes/Figure_A1/Figure_A1_0_data_rearrange2.R)
 * [1. Estimating Ideal Points](./Codes/Figure_A1/Figure_A1_1_ip_estimation.R)
 * [2. Exporting Figure/Table](./Codes/Figure_A1/Figure_A1_2_plot.R)

![Figure A1](https://github.com/tzuliu/Towards-a-General-Methodology-of-Bridging-Ideological-Spaces/raw/master/Outputs/application/utas09_figure.png)

### Figure A2: Density Plots of Senator's Positions with Different Amount of Extra Voter Samples

Replication Codes:
 * [0-1. Creating Data (0 extra samples)](./Codes/Figure_A5/Figure_A5_0_data_creation0000.R)
 * [0-2. Creating Data (10 extra samples)](./Codes/Figure_A5/Figure_A5_0_data_creation0010.R)
 * [0-3. Creating Data (20 extra samples)](./Codes/Figure_A5/Figure_A5_0_data_creation0020.R)
 * [0-4. Creating Data (50 extra samples)](./Codes/Figure_A5/Figure_A5_0_data_creation0050.R)
 * [0-5. Creating Data (100 extra samples)](./Codes/Figure_A5/Figure_A5_0_data_creation0100.R)
 * [1. Rearranging Data](./Codes/Figure_A5/Figure_A5_1_data_rearrange.R)
 * [2. Exporting Figure](./Codes/Figure_A5/Figure_A5_2_plot.R)

![Figure A2](https://github.com/tzuliu/Towards-a-General-Methodology-of-Bridging-Ideological-Spaces/raw/master/Outputs/simulation/senator_sim_400_outsample_density.png)