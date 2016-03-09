// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]

#define ARMA_DONT_USE_CXX11

#include <RcppArmadillo.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace arma;
using namespace RcppParallel;


/*** CDF gaussian dist - tuncated nomal */
double normCDF(double value)
{
  double a = 0.5 * erfc(-value * M_SQRT1_2);
  
  if(value>0){
    a = 0.5;
  }
  /* multiply by normalization constant*/
  a = a*2;
  
  return a;
}

/*** Probability of surviving to next period ***/
double pi_prob(double health, int t, double zeta, double omega, int T) {
  
  double probab = 0;
  
  if(health >= 0 && t <= T){
    probab = (1-exp(-zeta* pow(health, omega)));
  }
  
  return probab;
  
}

/*** Health depreciation ***/
double delta_dep(int t, double a0, double a1, double a2, double depre) {
  
  double x = exp(a0 + a1*t + a2*pow(t,2)) / (1 + exp(a0 + a1*t + a2*pow(t,2))); 
  
  return x;
  
}

/*** Investment function in health ***/
double invest(double doctor, double exercise, double rho, double kappa, double iota, double Omega, double prod_e) {
  
  double invest = 0;
  
  if(iota == 1.0){
    invest = Omega * pow((pow(doctor,rho)*pow((prod_e*exercise) ,1-rho)),kappa);
  }
  else{
    invest = Omega * pow(rho*pow(doctor,((iota-1)/iota))+(1-rho)*pow((prod_e*exercise),((iota-1)/iota)),((iota*kappa)/(iota-1)));
  }
  
  return invest;
  
}


/*** Sick time ***/
double sick(double Q, double psi, double health) {
  
  double x = Q * pow(health, -psi);
  
  if (x>0.999){
    x = 0.999;
  }
  
  return x;
  
}


/*** Probability of bad health shock ***/
double prob_shock(int t, std::vector<double> probs) {
  
  double x = 0;
  
  if(t<40){
    x = probs[0];
  }
  else{
    x = probs[t-40];
  }
  return x;
  
}


/*** Absolute value of elements of a vector ***/
std::vector<double> abs(std::vector<double> a) {
  
  std::vector<double> x = a;
  int siz = a.size();
  
  for(int i=0; i<siz; i++){
    if(x[i]<0){
      x[i] = -a[i];
    }
  }
  
  return x;
  
}

/*** Minimum entry of a vector ***/
int min(std::vector<double> V) {
  
  int siz = V.size();
  int min = 0;
  
  for(int i=0; i<siz; i++){
    if(V[i]<V[min]){
      min = i;
    }
  }
  return min;
  
}


/* Sigma_eps time dependent */
int sigma_eps(int t, std::vector<double> probs) {
  
  double x = 0;
  
  if(t<40){
    x = probs[0];
  }
  else{
    x = probs[t-40];
  }
  return x;
  
}


struct Vfi : public Worker{
  
  /* Parameters - constructors */
  const std::vector<double> hgrid;
  const std::vector<double> egrid; 
  const std::vector<double> bgrid;
  const std::vector<double> dgrid;
  const int T;
  const int nh;
  const int ne;
  const int nb;
  const int nd;
  const std::vector<double> w;
  const double r;
  const double P_d;
  const double theta;
  const double gamma;
  const double eta;
  const double xi;
  const double lambda;
  const double beta;
  const double Q;
  const double psi;
  const double a0;
  const double a1;
  const double a2;
  const double depre;
  const double zeta;
  const double omega;
  const double rho;
  const double kappa;
  const double iota;
  const double Omega;
  const double nu;
  const std::vector<double> probs;
  const double prod_e;
  const int reduction;
  const double morfina;
  const double health_limit;
  const double morf_mult;
  const double cost_e;
  const double expo_e;
  const int age;
  const std::vector<std::vector<std::vector<std::vector<double> > > > value2;
  
  /* Output matrix */
/*  std::vector<std::vector<std::vector<double> > > All_matrices;*/
  NumericVector Value;
  NumericVector Policyb;
  NumericVector Policyc;
  NumericVector Policyd;
  NumericVector Policye;
  NumericVector Policyl;
  
/*  Vfi(const vector<double> hgrid, const vector<double> egrid, const vector<double> bgrid, const vector<double> dgrid, const int T, const int nh, const int ne, const int nb, const int nd, const vector<double> w, const double r, const double P_d, const double theta, const double gamma, const double eta, const double xi, const double lambda, const double beta, const double Q, const double psi, const double a0, const double a1, const double a2, const double depre, const double zeta, const double omega, const double rho, const double kappa, const double iota, const double Omega, const double nu, const vector<double> probs, const double prod_e, const int reduction, const double morfina, const double health_limit, const double morf_mult, const double cost_e, const double expo_e, const int age, const vector<vector<vector<vector<double> > > > value2, vector<vector<vector<double> > > All_matrices)*/
  Vfi(const std::vector<double> hgrid, const std::vector<double> egrid, const std::vector<double> bgrid, const std::vector<double> dgrid, const int T, const int nh, const int ne, const int nb, const int nd, const std::vector<double> w, const double r, const double P_d, const double theta, const double gamma, const double eta, const double xi, const double lambda, const double beta, const double Q, const double psi, const double a0, const double a1, const double a2, const double depre, const double zeta, const double omega, const double rho, const double kappa, const double iota, const double Omega, const double nu, const std::vector<double> probs, const double prod_e, const int reduction, const double morfina, const double health_limit, const double morf_mult, const double cost_e, const double expo_e, const int age, const std::vector<std::vector<std::vector<std::vector<double> > > > value2, NumericVector Value, NumericVector Policyb, NumericVector Policyc, NumericVector Policyd, NumericVector Policye, NumericVector Policyl)
    : hgrid(hgrid), egrid(egrid), bgrid(bgrid), dgrid(dgrid), T(T), nh(nh), ne(ne), nb(nb), nd(nd), w(w), r(r), P_d(P_d), theta(theta), gamma(gamma), eta(eta), xi(xi), lambda(lambda), beta(beta), Q(Q), psi(psi), a0(a0), a1(a1), a2(a2), depre(depre), zeta(zeta), omega(omega), rho(rho), kappa(kappa), iota(iota), Omega(Omega), nu(nu), probs(probs), prod_e(prod_e), reduction(reduction), morfina(morfina), health_limit(health_limit), morf_mult(morf_mult), cost_e(cost_e), expo_e(expo_e), age(age), value2(value2), Value(Value), Policyb(Policyb), Policyc(Policyc), Policyd(Policyd), Policye(Policye), Policyl(Policyl) {}

  /* Aqui va el algortimo del VFI */
  void operator()(std::size_t begin, std::size_t end){
    
    double jump = hgrid[1] - hgrid[0];
    
    /* Initialize the matrix of tomorrow */
    std::vector<std::vector<double> > Value2;

    Value2.resize(nh);

    for(int j=0; j<nh; j++){
      Value2[j].resize(nb);

      for(int l=0; l<nb; l++){
        Value2[j][l] = pow(-10,25);
      }
    }
    
    
    std::vector<double> hp_h (1);
    std::vector<double> hp_m (1);
    std::vector<double> hp_l (1);
    std::vector<double> Vaux (nh);
    std::vector<double> Vexl (1);
    std::vector<double> Vexm (1);
    std::vector<double> Vexh (1);
    std::vector<double> Vex (1);
    double probabil;

        
    double c = pow(10,-20);
    double l = 0.0;
    double sick_t = 0.0;
    double VV = pow(-10,25);
    
    int ih;
    int ib;
    
    ih = 0;
    ib = 0;
    
    /*** Terminal period ***/
    if(age == T-1){
      
      for(int ipar=begin; ipar<end; ipar++){
        ih = floor(ipar/nb);
        ib = ipar % nb;

        if((1+r)*bgrid[ib] + w[age] > 0){
          Policyc[ipar] = (1+r)*bgrid[ib] + w[age];
        }
        
        if(1 - sick(Q, psi, hgrid[ih]) >=0 ){
          Policyl[ipar] = 1 - sick(Q, psi, hgrid[ih]);
        }
        
        if(((1+r)*bgrid[ib] + w[age] > 0) && (1 - sick(Q, psi, hgrid[ih]) >=0) ){
          if(theta != 1){
            Value[ipar] = (pow((lambda*pow(Policyc[ipar],1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)),((1-theta)/(1-xi))))/(1-theta);
          }
          else{
            /*** As in He and Huang ***/
            Value[ipar] = log((lambda*pow(Policyc[ipar],1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)))/(1-xi);
          }
        }

        Policyb[ipar] = 1;
        Policyd[ipar] = 0;
        Policye[ipar] = 0;
      }
    }
    
    
    /*** Other periods ***/ 
    
    if(age < T-1){
      
      for(int j=0; j<nh; j++){
        for(int l=0; l<nb; l++){
          Value2[j][l] = value2[age + 1][j][l][0];
        }
      }
      
      
      for(int ipar=begin; ipar<end; ipar++){
        ih = floor(ipar/nb);
        ib = ipar % nb;
        
        for(int iep=0; iep<ne; iep++){
          for(int ibp=0; ibp<nb; ibp++){
            for(int idp=0; idp<nd; idp++){
              
              c = pow(10,-20);
              l = 0.0;
              VV = pow(-10,25);
              
              if((1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp] > 0){
                c = (1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp];
              }
              
              sick_t = sick(Q, psi, hgrid[ih]);
              if(1 - sick_t - egrid[iep]>=0){
                l = 1 - sick_t - egrid[iep];
              }
              
              Vex[0] = 0.0;
              for(int i=0; i<nh; i++){
                if(i==0){
                  probabil = normCDF((hgrid[i]+(jump/2)-(1-delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih]-invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e))/sigma_eps(age, probs));
                }
                else if(i==nh-1){
                  probabil = 1 - normCDF((hgrid[i]-(jump/2)-(1-delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih]-invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e))/sigma_eps(age, probs));
                }
                else {
                  probabil = normCDF((hgrid[i]+(jump/2)-(1-delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih]-invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e))/sigma_eps(age, probs)) - normCDF((hgrid[i]-(jump/2)-(1-delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih]-invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e))/sigma_eps(age, probs));
                }
                Vex[0] = Vex[0]+ Value2[i][ibp]*probabil;
              }
              
              if( ( (1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp] > 0) && (1 - sick_t - egrid[iep]>=0) ){
                
                if(theta != 1){
                  VV = (pow(lambda*pow(c,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi),((1-theta)/(1-xi))))/(1-theta) - cost_e*pow(egrid[iep], (1+expo_e))/(1+expo_e) + beta * pow(pi_prob(hgrid[ih], age+1, zeta, omega, T),reduction) * Vex[0];
                }
                else{
                  VV = log(lambda*pow(c,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi))/(1-xi)                         - cost_e*pow(egrid[iep], (1+expo_e))/(1+expo_e) + beta * pow(pi_prob(hgrid[ih], age+1, zeta, omega, T),reduction) * Vex[0];
                }
                
              }

              /*** The maximum ***/
              if(VV >= Value[ipar]){
                Value[ipar] = VV;
                Policyc[ipar] = c;
                Policyl[ipar] = l;
                Policyb[ipar] = ibp+1;
                Policyd[ipar] = dgrid[idp];
                Policye[ipar] = egrid[iep];
              }
            }
          }
        }
      }
    }

  }
  
};


/*** THE FUNCTION ***/
// [[Rcpp::export]]
std::vector<std::vector<std::vector<std::vector<double> > > > vfi_parallel(std::vector<double> hgrid, std::vector<double> egrid, std::vector<double> bgrid, std::vector<double> dgrid, int T, int nh, int ne, int nb, int nd, std::vector<double> w, double r, double P_d, double theta, double gamma, double eta, double xi, double lambda, double beta, double Q, double psi, double a0, double a1, double a2, double depre, double zeta, double omega, double rho, double kappa, double iota, double Omega, double nu, std::vector<double> probs, double prod_e, int reduction, double morfina, double health_limit, double morf_mult, double cost_e, double expo_e) {  
  /*** Initializing ***/

  std::vector<std::vector<std::vector<std::vector<double> > > > All_matrices;
  std::vector<std::vector<std::vector<double> > > Value;

  All_matrices.resize(T);
  Value.resize(T);
  
  for(int i=0; i<T; i++){
    All_matrices[i].resize(nh);
    Value[i].resize(nh);
    for(int j=0; j<nh; j++){
      All_matrices[i][j].resize(nb);
      Value[i][j].resize(nb);
      for(int l=0; l<nb; l++){
        All_matrices[i][j][l].resize(6);
      }
    }
  }

  int ih;
  int ib;
  

  for(int age = T-1; age>=0; age--){
    Rcout << "Age is:" << age+1 << std::endl;

    NumericVector Value_out(nh*nb, pow(-10, 25)); 
    NumericVector Polb_out(nh*nb); 
    NumericVector Polc_out(nh*nb, pow(10, -20)); 
    NumericVector Pold_out(nh*nb); 
    NumericVector Pole_out(nh*nb); 
    NumericVector Poll_out(nh*nb); 

    Vfi vfi(hgrid, egrid, bgrid, dgrid, T, nh, ne, nb, nd, w, r, P_d, theta, gamma, eta, xi, lambda, beta, Q, psi, a0, a1, a2, depre, zeta, omega, rho, kappa, iota, Omega, nu, probs, prod_e, reduction, morfina, health_limit, morf_mult, cost_e, expo_e, age, All_matrices, Value_out, Polb_out, Polc_out, Pold_out, Pole_out, Poll_out);
    
    // call it with parallelFor
    parallelFor(0, nh*nb, vfi);
    
    for(int ipar=0; ipar<nb*nh; ipar++){
      ih = floor(ipar/nb);
      ib = ipar % nb;
      All_matrices[age][ih][ib][0]=Value_out(ipar);
      All_matrices[age][ih][ib][1]=Polb_out(ipar);
      All_matrices[age][ih][ib][2]=Polc_out(ipar);
      All_matrices[age][ih][ib][3]=Pold_out(ipar);
      All_matrices[age][ih][ib][4]=Pole_out(ipar);
      All_matrices[age][ih][ib][5]=Poll_out(ipar);
    }
    
  }
  
  
  return All_matrices;
}

