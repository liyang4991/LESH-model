# LESH-model

This code is for the [LESH model](https://www.tandfonline.com/doi/full/10.1080/17538947.2023.2271883) :'A locally explained heterogeneity model for examining wetland disparity'.

![image](https://github.com/user-attachments/assets/d51edc97-d62d-4b81-ad8b-908e001a660c)
<center> The workflow of the LESH model </center>

## usage
"Fun_treeall.R" is a R function file for [GOZH model](https://www.sciencedirect.com/science/article/pii/S0924271622000132). 

"Fun_shap.R" is a R function file for LESH model.

"run_lesh.R" is a unified entry that runs GOZH + LESH in one call.

"sim.Rmd" provides an example. A simulation data is used to demonstrate the process of calculating SPD.

### Quick start
```r
source("run_lesh.R")

sim_1 <- read.csv("sim_data182uniform.csv")
res <- run_lesh(
  data = sim_1,
  formula = y ~ X1 + X2 + X3,
  complexity = 0.02,
  vars = c("X1", "X2", "X3"),
  location = c("X")
)

res$shap_values
head(res$all.q)
```


## citation
If you are using the code/model provided here in a publication, please consider citing:

```{BibTex}
@article{li2023locally,
  title={A locally explained heterogeneity model for examining wetland disparity},
  author={Li, Yang and Luo, Peng and Song, Yongze and Zhang, Liqiang and Qu, Ying and Hou, Zhengyang},
  journal={International Journal of Digital Earth},
  volume={16},
  number={2},
  pages={4533--4552},
  year={2023},
  publisher={Taylor \& Francis}
}
```


## contact
For any questions, please contact me via e-mail: isliyang@mail.bnu.edu.cn.
