(*
  Correction of the call-by-value Krivine Abstract Machine.
  Author: Rodolphe Lepigre <rodolphe.lepigre@univ-smb.fr>

  Developped with the Coq Proof Assistant, version 8.6 (February 2017).
*)

(* Relation closure. *)
Inductive TClosure T (R : T ->T ->Prop) : T ->T ->Prop :=
  | TStep : forall t1 t2, R t1 t2 ->TClosure T R t1 t2
  | TTran : forall t1 t2 t3, TClosure T R t1 t2 ->R t2 t3 ->TClosure T R t1 t3.

Theorem TTran_aux : forall T R t1 t2 t3,
  TClosure T R t1 t2 ->TClosure T R t2 t3 ->TClosure T R t1 t3.
Proof.
  intros T R t1 t2 t3 H1 H2. induction H2.
  - apply TTran with t0. exact H1. exact H.
  - apply TTran with t2. apply IHTClosure. exact H1. exact H.
Qed.

Inductive RTClosure T (R : T -> T -> Prop) : T -> T -> Prop :=
  | RTStep : forall t1 t2, R t1 t2 ->RTClosure T R t1 t2
  | RTRefl : forall t, RTClosure T R t t
  | RTTran : forall t1 t2 t3, RTClosure T R t1 t2 ->R t2 t3 ->RTClosure T R t1 t3.

Theorem RTTran_aux : forall T R t1 t2 t3,
  RTClosure T R t1 t2 ->RTClosure T R t2 t3 ->RTClosure T R t1 t3.
Proof.
  intros T R t1 t2 t3 H1 H2. induction H2.
  - apply RTTran with t0. exact H1. exact H.
  - exact H1.
  - apply RTTran with t2. apply IHRTClosure. exact H1. exact H.
Qed.

Theorem split_transitive_closure :
  forall T R t1 t2, TClosure T R t1 t2 ->exists u,  R t1 u /\RTClosure T R u t2.
Proof.
  intros T R t1 t2 H. induction H.
  - exists t2. split. exact H. apply RTRefl.
  - destruct IHTClosure as (u, IH). destruct IH as (IH1, IH2). exists u.
    split. exact IH1. apply RTTran with t2. exact IH2. exact H0.
Qed.

Lemma to_transitive_closure_aux :
  forall T R u1 u2, RTClosure T R u1 u2 ->forall u3, R u2 u3 ->TClosure T R u1 u3.
Proof.
  intros T R u1 u2 H1.
  induction H1; intros u3 H2.
  - apply TTran with t2. apply TStep. exact H. exact H2.
  - apply TStep. exact H2.
  - specialize IHRTClosure with t3. apply TTran with t3.
    apply IHRTClosure. exact H. exact H2.
Qed.

Theorem to_transitive_closure :
  forall T R t1 t2, RTClosure T R t1 t2 ->t1 = t2 \/TClosure T R t1 t2.
Proof.
  intros T R t1 t2 H. induction H.
  - right. apply TStep. exact H.
  - left. reflexivity.
  - right. induction IHRTClosure.
    + apply (to_transitive_closure_aux T R t1 t2 H t3 H0).
    + apply TTran with t2. exact H1. exact H0.
Qed.

(* AST of the call-by-value lambda calculus. *)
Inductive valu {var} : Type :=
  | Var : var -> valu
  | Abs : (var -> term) -> valu
with term {var} : Type :=
  | val : valu -> term
  | app : term -> term -> term.

Definition Valu := forall var, @valu var.
Definition Term := forall var, @term var.

Definition App (t : Term) (u : Term) : Term :=
  fun _ => app (@t _) (@u _).

Definition Val (v : Valu) : Term :=
  fun _ => val (@v _).

(* Substitution. *)
Fixpoint squash_valu var (v : @valu (@valu var)) : @valu var :=
  match v with
    | Var x => x
    | Abs b => Abs (fun x => squash_term var (b (Var x)))
  end
with squash_term var (t : @term (@valu var)) : @term var :=
  match t with
    | val v   => val (squash_valu var v)
    | app t u => app (squash_term var t) (squash_term var u)
  end.

Definition Subst (b : forall var, var -> @term var) (v : Valu) : Term :=
  fun var => squash_term var (b (@valu var) (@v var)).

(* Term examples. *)
Example id   : Valu := fun var => @Abs var (fun x => @val var (Var x)).
Example idid : Term := App (Val id) (Val id).
Example fst  : Valu := fun var => @Abs var (fun x => @val var (@Abs var (fun y => @val var (Var x)))).
Example snd  : Valu := fun var => @Abs var (fun x => @val var (@Abs var (fun y => @val var (Var y)))).

(* Definition of the reduction relation. *)
Inductive reduces_one : Term -> Term -> Prop :=
  | Red_right : forall t u1 u2, reduces_one u1 u2 -> reduces_one (App t u1) (App t u2)
  | Red_left  : forall t1 t2 v, reduces_one t1 t2 -> reduces_one (App t1 (Val v)) (App t2 (Val v))
  | Red_Beta  : forall b v, reduces_one (App (Val (fun var => Abs (b var))) (Val v)) (Subst b v).

Definition reduces_many := RTClosure Term reduces_one.
Definition reduces_many_plus := TClosure Term reduces_one.

Notation "t --> u" := (reduces_one t u) (at level 80, right associativity).
Notation "t -->* u" := (reduces_many t u) (at level 80, right associativity).
Notation "t -->+ u" := (reduces_many_plus t u) (at level 80, right associativity).

(* Stack and abstract machine. *)
Inductive Stack : Type :=
  | Eps : Stack
  | Psh : Valu -> Stack -> Stack
  | Frm : Term -> Stack -> Stack.

Inductive Proc : Type :=
  | Prc : Term -> Stack -> Proc.

(* Machine reduction. *)
Inductive KAM_weak_one : Proc ->Proc -> Prop :=
  | Store : forall t u pi, KAM_weak_one (Prc (App t u) pi) (Prc u (Frm t pi))
  | Swap  : forall v t pi, KAM_weak_one (Prc (Val v) (Frm t pi)) (Prc t (Psh v pi)).

Definition KAM_weak_many := RTClosure Proc KAM_weak_one.
Definition KAM_weak_many_plus := TClosure Proc KAM_weak_one.

Notation "p <~ q" := (KAM_weak_one q p) (at level 80, right associativity).
Notation "p ~> q" := (KAM_weak_one p q) (at level 80, right associativity).
Notation "p *<~ q" := (KAM_weak_many q p) (at level 80, right associativity).
Notation "p ~>* q" := (KAM_weak_many p q) (at level 80, right associativity).
Notation "p +<~ q" := (KAM_weak_many_plus q p) (at level 80, right associativity).
Notation "p ~>+ q" := (KAM_weak_many_plus p q) (at level 80, right associativity).

Inductive KAM_beta : Proc ->Proc ->Prop :=
  | Pop   : forall b v pi, KAM_beta (Prc (Val (fun var => Abs (b var))) (Psh v pi)) (Prc (Subst b v) pi).

Notation "p >>B q" := (KAM_beta p q) (at level 80, right associativity).

Inductive KAM_step : Proc ->Proc ->Prop :=
  | Step : forall p q r, KAM_weak_many p q ->KAM_beta q r ->KAM_step p r.

Definition KAM_steps_plus := TClosure Proc KAM_step.
Definition KAM_steps := RTClosure Proc KAM_step.

Notation "p >> q" := (KAM_step p q) (at level 80, right associativity).
Notation "p >>* q" := (KAM_steps p q) (at level 80, right associativity).
Notation "p >>+ q" := (KAM_steps_plus p q) (at level 80, right associativity).

(* Push weak step into normal step. *)
Theorem weak_many_to_step : forall p q r, (p ~>* q) ->(q >> r) ->(p >> r).
Proof.
  intros p q r H1 H2. destruct H2.
  assert (p ~>* q). apply RTTran_aux with p0. exact H1. exact H.
  apply (Step p q r H2 H0).
Qed.
  
Theorem weak_one_to_step : forall p q r, (p ~> q) ->(q >> r) ->(p >> r).
Proof.
  intros p q r H1 H2. apply weak_many_to_step with q.
  apply RTStep. exact H1. exact H2.
Qed.

Theorem weak_many_to_steps_plus : forall p q r, (p ~>* q) ->(q >>+ r) ->(p >>+ r).
Proof.
  intros p q r H1 H2. induction H2.
  - apply TStep. apply (weak_many_to_step p t1 t2 H1 H).
  - fold KAM_steps_plus in H2. rename IHTClosure into IH.
    apply TTran with t2. apply IH. exact H1. exact H.
Qed.

Theorem weak_one_to_steps_plus : forall p q r, (p ~> q) ->(q >>+ r) ->(p >>+ r).
Proof.
  intros p q r H1 H2. apply weak_many_to_steps_plus with q.
  apply RTStep. exact H1. exact H2.
Qed.

(* One step simulation. *)  
Theorem sim_one :
  forall t u, (t --> u) ->forall pi, exists p, (p *<~ (Prc u pi)) /\((Prc t pi) >> p).
Proof.
  intros t u H.
  induction H; intros pi.
  - rename IHreduces_one into IH. specialize IH with (pi := Frm t pi).
    destruct IH as (p, IH). destruct IH as (IH1, IH2). exists p. split.
    + apply RTTran_aux with (Prc u2 (Frm t pi)). apply RTStep. constructor. exact IH1.
    + apply (weak_one_to_step (Prc (App t u1) pi) (Prc u1 (Frm t pi)) p).
      constructor. exact IH2.
  - rename IHreduces_one into IH. specialize IH with (pi := Psh v pi).
    destruct IH as (p, IH). destruct IH as (IH1, IH2). exists p. split.
    + apply RTTran_aux with (Prc (Val v) (Frm t2 pi)). apply RTStep. constructor.
      apply RTTran_aux with (Prc t2 (Psh v pi)). apply RTStep. constructor. exact IH1.
    + apply (weak_many_to_step (Prc (App t1 (Val v)) pi) (Prc t1 (Psh v pi)) p).
      apply RTTran_aux with (Prc (Val v) (Frm t1 pi)). apply RTStep. constructor.
      apply RTStep. constructor. exact IH2.
  - exists (Prc (Subst b v) pi). split.
    + apply RTRefl.
    + assert (Prc (App (Val (fun var =>Abs (b var))) (Val v)) pi ~>* Prc (Val (fun var =>Abs (b var))) (Psh v pi)).
      apply RTTran_aux with (Prc (Val v) (Frm (Val (fun var =>Abs (b var))) pi)). apply RTStep. constructor.
      apply RTStep. constructor.
      apply (Step _ _ _ H (Pop b v pi)).
Qed.
  
Lemma stupid : forall t u v, ~ (App t u = Val v).
Proof.
  intros. intro.
  assert (App t u False = Val v False) as R. rewrite H. reflexivity.
  unfold App. unfold App in R. unfold Val in R. discriminate R.
Qed.

Lemma beta_not_weak : forall p q, (p >>B q) ->forall r, ~(p ~> r).
Proof.
  intros p q H1 r H2. destruct H1. inversion H2.
  pose proof stupid t u (fun var =>Abs (b var)) as R.
  apply R. exact H0.
Qed.

(* Variation of function extensionality. *)
Axiom Ext : forall{A : Type ->Type} {P1 P2 : forall(X : Type), A X}, (forall (x : Type), P1 x = P2 x) ->P1 = P2.

Lemma ExtT : forall{t1 t2 : Term}, (forall (x : Type), t1 x = t2 x) ->t1 = t2.
Proof.
  intros. apply (Ext H).
Qed.
  
Lemma ExtV : forall{v1 v2 : Valu}, (forall (x : Type), v1 x = v2 x) ->v1 = v2.
Proof.
  intros. apply (Ext H).
Qed.

Lemma split_forall : forall{f g : Type ->Prop}, (forall x, f x /\g x) ->(forall x, f x) /\(forall x, g x).
Proof.
  intros. split.
  - intro. specialize H with x. destruct H as (H1, H2). exact H1.
  - intro. specialize H with x. destruct H as (H1, H2). exact H2.
Qed.

Lemma injection_app_aux : forall t1 u1 t2 u2, App t1 u1 = App t2 u2 ->
  forall var, @t1 var = @t2 var /\ @u1 var = @u2 var.
Proof.
  intros.
  assert (App t1 u1 var = App t2 u2 var). rewrite H. reflexivity.
  unfold App in H0. injection H0. intros E1 E2.
  split. exact E2. exact E1.
Qed.

Lemma injection_app : forall t1 u1 t2 u2, App t1 u1 = App t2 u2 ->t1 = t2 /\u1 = u2.
Proof.
  intros. pose proof injection_app_aux t1 u1 t2 u2 H.
  pose proof split_forall H0. destruct H1 as (L, R). split.
  + apply (ExtT L).
  + apply (ExtT R).
Qed.
  
Lemma injection_val_aux : forall v1 v2, Val v1 = Val v2 ->forall var, @v1 var = @v2 var.
Proof.
  intros.
  assert (Val v1 var = Val v2 var). rewrite H. reflexivity.
  unfold Val in H0. injection H0. intro. exact H1.
Qed.

Lemma injection_val : forall v1 v2, Val v1 = Val v2 ->v1 = v2.
Proof.
  intros. pose proof injection_val_aux v1 v2 H as L. apply (ExtV L).
Qed.

Lemma weak_determinist : forall p q1 q2, (p ~> q1) ->(p ~> q2) ->q1 = q2.
Proof.
  intros p q1 q2 H1 H2. generalize dependent q2. induction H1.
  - intros q2 H2. inversion H2.
    + subst. pose proof injection_app t0 u0 t u H0 as E. destruct E as (E1, E2).
      rewrite E1. rewrite E2. reflexivity.
    + pose proof stupid t u v. symmetry in H0. pose proof H H0 as R.
      exfalso. exact R.
  - intros q2 H2. inversion H2.
    + pose proof stupid t0 u v. pose proof H H0. exfalso. exact H4.
    + subst. pose proof injection_val v0 v H0. rewrite H. reflexivity.
Qed.

Lemma can_step_plus : forall p q r, (p ~> r) ->(p ~>+ q) ->(forall s, ~(q ~> s)) ->(r ~>* q).
Proof.
  intros p q r H1 H2 H3.
  pose proof split_transitive_closure Proc KAM_weak_one p q H2 as S.
  destruct S as (u, (S1, S2)).
  pose proof weak_determinist p r u H1 S1. rewrite <-H in S2. exact S2.
Qed.

Lemma can_step : forall p q r, (p ~> r) ->(p ~>* q) ->(forall s, ~(q ~> s)) ->(r ~>* q).
Proof.
  intros p q r H1 H2 H3.
  pose proof to_transitive_closure Proc KAM_weak_one p q H2 as Lem.
  induction Lem.
  - rewrite H in H1. specialize H3 with r. pose proof H3 H1. exfalso. exact H0.
  - apply (can_step_plus p q r H1 H H3).
Qed.

Lemma sim_many_aux_aux : forall p r q, (p <~ r) ->(r >> q) ->(p >> q).
Proof.
  intros p r q E H.
  induction E.
  - inversion H.
    pose proof beta_not_weak q0 q H1 as Lem.
    assert (Prc u (Frm t pi) ~>* q0).
    apply (can_step _ _ _ (Store _ _ _) H0 Lem).
    apply (Step (Prc u (Frm t pi)) q0 q H4 H1).
  - inversion H.
    pose proof beta_not_weak q0 q H1 as Lem.
    assert (Prc t (Psh v pi) ~>* q0).
    apply (can_step _ _ _ (Swap _ _ _) H0 Lem).
    apply (Step (Prc t (Psh v pi)) q0 q H4 H1).
Qed.

Lemma sim_many_aux : forall p r q, (p *<~ r) ->(r >> q) ->(p >> q).
Proof.
  intros p r q E H.
  induction E.
  - apply (sim_many_aux_aux t2 t1 q H0 H).
  - exact H.
  - fold KAM_weak_many in E. apply (sim_many_aux_aux t3 t2 q). exact H0. 
    apply IHE. exact H.
Qed.
  
Theorem sim_many :
  forall t u, (t -->* u) ->forall pi, exists p, (p *<~ (Prc u pi)) /\((Prc t pi) >>* p).
Proof.
  intros t u H pi. induction H.
  - pose proof sim_one t1 t2 H pi as HThm.
    destruct HThm as (p, HThm). destruct HThm as (HThm1, HThm2). exists p. split.
    + exact HThm1.
    + apply RTStep. exact HThm2.
  - exists (Prc t pi). split.
    + apply RTRefl.
    + apply RTRefl.
  - rename IHRTClosure into IH. fold reduces_many in H0.
    pose proof sim_one t2 t3 H0 pi as HThm.      
    destruct IH as (p, IH). destruct IH as (IH1, IH2).
    destruct HThm as (q, HThm). destruct HThm as (HThm1, HThm2).
    exists q. split.
    + exact HThm1.
    + apply RTTran with p. exact IH2.
      fold reduces_many in H.
      apply (sim_many_aux p (Prc t2 pi) q IH1 HThm2).  
Qed.
