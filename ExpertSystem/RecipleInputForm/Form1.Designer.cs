namespace RecipleInputForm
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.txbName = new System.Windows.Forms.TextBox();
            this.lbName = new System.Windows.Forms.Label();
            this.btnInput = new System.Windows.Forms.Button();
            this.numMake = new System.Windows.Forms.NumericUpDown();
            this.lstUnknown = new System.Windows.Forms.ListBox();
            this.button1 = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            ((System.ComponentModel.ISupportInitialize)(this.numMake)).BeginInit();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // txbName
            // 
            this.txbName.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend;
            this.txbName.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource;
            this.txbName.Location = new System.Drawing.Point(3, 3);
            this.txbName.Name = "txbName";
            this.txbName.Size = new System.Drawing.Size(186, 20);
            this.txbName.TabIndex = 0;
            // 
            // lbName
            // 
            this.lbName.AutoSize = true;
            this.lbName.Location = new System.Drawing.Point(2, 9);
            this.lbName.Name = "lbName";
            this.lbName.Size = new System.Drawing.Size(35, 13);
            this.lbName.TabIndex = 1;
            this.lbName.Text = "Name";
            // 
            // btnInput
            // 
            this.btnInput.Location = new System.Drawing.Point(275, 28);
            this.btnInput.Name = "btnInput";
            this.btnInput.Size = new System.Drawing.Size(75, 23);
            this.btnInput.TabIndex = 2;
            this.btnInput.TabStop = false;
            this.btnInput.Text = "Input";
            this.btnInput.UseVisualStyleBackColor = true;
            // 
            // numMake
            // 
            this.numMake.Location = new System.Drawing.Point(195, 4);
            this.numMake.Maximum = new decimal(new int[] {
            100000,
            0,
            0,
            0});
            this.numMake.Name = "numMake";
            this.numMake.Size = new System.Drawing.Size(45, 20);
            this.numMake.TabIndex = 3;
            this.numMake.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // lstUnknown
            // 
            this.lstUnknown.FormattingEnabled = true;
            this.lstUnknown.Location = new System.Drawing.Point(356, 29);
            this.lstUnknown.Name = "lstUnknown";
            this.lstUnknown.Size = new System.Drawing.Size(120, 173);
            this.lstUnknown.TabIndex = 4;
            this.lstUnknown.TabStop = false;
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(275, 57);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(75, 23);
            this.button1.TabIndex = 5;
            this.button1.TabStop = false;
            this.button1.Text = "Expand";
            this.button1.UseVisualStyleBackColor = true;
            // 
            // panel1
            // 
            this.panel1.AutoScroll = true;
            this.panel1.Controls.Add(this.txbName);
            this.panel1.Controls.Add(this.numMake);
            this.panel1.Location = new System.Drawing.Point(2, 25);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(256, 271);
            this.panel1.TabIndex = 6;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(495, 308);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.lstUnknown);
            this.Controls.Add(this.btnInput);
            this.Controls.Add(this.lbName);
            this.Name = "Form1";
            this.Text = "Form1";
            ((System.ComponentModel.ISupportInitialize)(this.numMake)).EndInit();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label lbName;
        public System.Windows.Forms.Button btnInput;
        public System.Windows.Forms.NumericUpDown numMake;
        public System.Windows.Forms.TextBox txbName;
        public System.Windows.Forms.ListBox lstUnknown;
        public System.Windows.Forms.Button button1;
        public System.Windows.Forms.Panel panel1;
    }
}

