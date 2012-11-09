DATA_SECTION
   init_int n;                        // number of distances
   init_number width;
   init_vector xs(1,n);               // distances
PARAMETER_SECTION
   init_number beta;                  // beta parameter for log-sigma;
   init_bounded_number sigeps(0.000001,5);   
                                      // sigma for random effect;        
   random_effects_vector u(1,n);      // random effect for scale
   objective_function_value f;        // negative log-likelihood
GLOBALS_SECTION
  #include <admodel.h>
  dvariable sigma;

PROCEDURE_SECTION
   int j;
// loop over each observation computing sum of log-likelihood values
// integral is sqrt(pi/2)*sigma; I dropped constant
   f=0;
   for (j=1;j<=n;j++)
   {
      ll_j(j,beta,sigeps,u(j));
   }  

SEPARABLE_FUNCTION void ll_j(const int j, const dvariable& beta,const dvariable& sigeps,const dvariable& u)
   dvariable eps=u*sigeps;
   int k;
   dvariable mu;
   sigma=exp(beta+eps);
   mu=adromb(&model_parameters::fct,0,width,8);
   f -= -0.5*square(u);
   f -= -log(mu) - 0.5*square(xs(j)/sigma);

FUNCTION dvariable fct(const dvariable& x)
// x is integration variable
// ifct is index for function read from data
   dvariable tmp;
   tmp=mfexp(-.5*x*x/square(sigma));
   return tmp;



